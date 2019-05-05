{-# LANGUAGE OverloadedStrings #-}
module App where

import           Protolude                      ( IO
                                                , ($)
                                                , (.)
                                                , (>>=)
                                                , ($>)
                                                , Applicative
                                                , either
                                                , (&)
                                                , pure
                                                , liftIO
                                                , const
                                                , Maybe(..)
                                                , Either(..)
                                                , show
                                                , MonadIO
                                                , putStrLn
                                                , (<>)
                                                )
import qualified Control.Exception             as E
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Except     ( runExceptT )
import           Control.Monad.Trans            ( lift )
import qualified Crypto                        as Crypto
import qualified Crypto.Argon2                 as Argon2
import           Data.Default                   ( def )
import           Data.Text.Lazy                 ( unpack )
import           Data.ByteString                ( ByteString )
import           Data.String                    ( String )
import           Domain                         ( newUser
                                                , createAccessToken
                                                )
import           Hasql.Pool                    as HP
import           Network.Wai                    ( Middleware )
import           Network.Wai.Middleware.Rewrite ( PathsAndQueries
                                                , rewritePureWithQueries
                                                )
import           Network.Wai.Middleware.Gzip    ( gzip )
import           Network.HTTP.Types.Header      ( RequestHeaders )
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans
import qualified API.Types                     as API
import qualified Types                         as T
import qualified Conf                          as Conf
import           Conf                           ( Environment(..) )
import qualified Data.Configurator             as C
import qualified Schema                        as Schema
import qualified Data.Text.Lazy                as LT
import qualified Queries                       as Q

app :: Conf.Environment -> IO ()
app env = void $ runMaybeT $ do
  putStrLn
    ("Loading application config with environment: " <> (show env) :: String)
  conf <- MaybeT $ do
    loaded <- C.load [C.Required $ unpack $ Conf.confFileName env]
    putStrLn ("Loading config file" :: String)
    parsed <- Conf.makeConfig loaded
    case parsed of
      Just _ -> do
        putStrLn ("Parsed config file" :: String)
        pure parsed
      Nothing ->
        putStrLn ("Parsed config file, but found nothing" :: String) $> Nothing
  putStrLn ("Acquiring database connection pool" :: String)
  lift $ E.bracket
    ( HP.acquire
    $ Conf.connectInfo (Conf.databaseConfig conf) Conf.applicationAccount
    )
    HP.release
    (\conn -> do
      putStrLn ("Connection pool acquired, preparing initial state" :: String)
      migrationResult <- Schema.runMigrations
        (Conf.migrationsPath $ Conf.databaseConfig conf)
        conn
      _     <- either (fail . show) pure migrationResult
      store <- Crypto.createStoreOrFail
      let initialAppState = T.AppState store (Conf.signingKey conf) conn
      putStrLn ("Initial state established, starting scotty app" :: String)
      let logger = Conf.logger env
      let runActionToIO m = runReaderT (runWebM m) initialAppState
      scottyT 4000 runActionToIO $ app' logger
    )

{- A MonadTrans-like Monad for our application.
  Its kind is too refined however to allow this to have a MonadTrans instance
-}
newtype WebM a = WebM { runWebM :: ReaderT T.AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader T.AppState)

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

type ActionT' = ActionT LT.Text WebM

app' :: Middleware -> ScottyT LT.Text WebM ()
app' logger = do
  middleware $ rewritePureWithQueries removeApiPrefix
  middleware logger
  middleware $ gzip def
  post "/login" $ do
    loginUserReq <- jsonData :: ActionT' API.LoginUser
    desiredUser  <- lift $ Schema.runStatement (loginUserReq ^. API.username)
                                        Q.findUserByUsername
    case desiredUser of
      Right (Just (Schema.Timestamped _ _ user)) -> do
        let correctPassword  = user ^. Schema.userHashedPassword
        let providedPassword = loginUserReq ^. API.rawPassword
        let passwordVerificationResult =
              Crypto.verifyPassword correctPassword providedPassword
        case passwordVerificationResult of
          Argon2.Argon2Ok -> respondWithAuthToken (user ^. Schema.userUserId)
          _               -> status status400
      _ -> status status400

  post "/register" $ do
    registerUserReq <- jsonData :: ActionT' API.RegisterUser
    salt            <- lift $ Crypto.nextBytes 16
    user            <-
      newUser registerUserReq (T.PasswordSalt salt)
      &   runExceptT
      &   liftIO
      >>= either E.throw pure
    result <- lift $ Schema.runStatement user Q.insertUser
    case result of
      Left err -> do
        putStrLn (show err :: ByteString)
        status status400
      Right _ ->
        respondWithAuthToken (user ^. Schema.createT . Schema.userUserId)

respondWithAuthToken :: T.UserId -> ActionT' ()
respondWithAuthToken userId = do
  token           <- liftIO $ createAccessToken userId
  tokenSigningKey <- webM $ asks (^. T.signingKey)
  either (const (status status500)) json (Crypto.signJwt tokenSigningKey token)


removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix ("api" : tail, queries) _ = (tail, queries)
removeApiPrefix paq                     _ = paq
