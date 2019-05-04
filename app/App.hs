{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module App where

import           Protolude                      ( IO
                                                , ($)
                                                , (.)
                                                , (>>=)
                                                , ($>)
                                                , (^)
                                                , Either
                                                , Applicative
                                                , either
                                                , (&)
                                                , pure
                                                , liftIO
                                                , const
                                                , Int
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
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                )
import           Control.Monad.Trans            ( lift )
import           Crypto.Classes.Exceptions      ( genBytes )
import           Crypto                         ( signJwt
                                                , verifyPassword
                                                )
import qualified Crypto.Argon2                 as Argon2
import           Data.Default                   ( def )
import           Data.Text.Lazy                 ( unpack )
import           Data.ByteString                ( ByteString )
import           Data.String                    ( String )
import           Domain                         ( newUser
                                                , createAccessToken
                                                )
import           Control.Concurrent.STM         ( atomically
                                                , readTVarIO
                                                , modifyTVar'
                                                , newTVarIO
                                                )
import qualified Hasql.Session                 as Session
import qualified Hasql.Statement               as Statement
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
import           System.Entropy                 ( getEntropy )
import           Crypto.Random.DRBG             ( newGenAutoReseed )
import qualified Data.Text.Lazy                as LT
import           Crypto.Random                  ( GenError )
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
      _                 <- either (fail . show) pure migrationResult
      eitherErrAppState <- runExceptT (generateInitialAppState conf)
      initialAppState   <- either E.throwIO return eitherErrAppState
      putStrLn ("Initial state established, starting scotty app" :: String)
      let logger = Conf.logger env
      let runActionToIO m = runReaderT (runWebM m) initialAppState
      scottyT 4000 runActionToIO $ app' conn logger
    )

{- A MonadTrans-like Monad for our application.
  Its kind is too refined however to allow this to have a MonadTrans instance
-}
newtype WebM a = WebM { runWebM :: ReaderT T.AppState IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader T.AppState)

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

generateInitialAppState :: Conf.Config -> ExceptT GenError IO T.AppState
generateInitialAppState conf = do
  initialEntropy <- lift $ getEntropy 256
  gen            <- ExceptT $ return (newGenAutoReseed initialEntropy (2 ^ 48))
  genVar         <- lift $ newTVarIO gen
  return $ T.AppState genVar (Conf.signingKey conf)

type ActionT' = ActionT LT.Text WebM

runStatement
  :: MonadIO m
  => HP.Pool
  -> a
  -> Statement.Statement a b
  -> m (Either HP.UsageError b)
runStatement pool p statement =
  liftIO $ HP.use pool (Session.statement p statement)

app' :: HP.Pool -> Middleware -> ScottyT LT.Text WebM ()
app' pool logger = do
  middleware $ rewritePureWithQueries removeApiPrefix
  middleware logger
  middleware $ gzip def
  let runStatement' = runStatement pool
  post "/login" $ do
    loginUserReq <- jsonData :: ActionT' API.LoginUser
    desiredUser  <- runStatement' (loginUserReq ^. API.username)
                                  Q.findUserByUsername
    case desiredUser of
      Right (Just (Schema.Timestamped _ _ user)) -> do
        let correctPassword  = user ^. Schema.userHashedPassword
        let providedPassword = loginUserReq ^. API.rawPassword
        let passwordVerificationResult =
              verifyPassword correctPassword providedPassword
        case passwordVerificationResult of
          Argon2.Argon2Ok -> respondWithAuthToken (user ^. Schema.userUserId)
          _               -> status status400
      _ -> status status400

  post "/register" $ do
    registerUserReq <- jsonData :: ActionT' API.RegisterUser
    salt            <- nextBytes 16
    user            <-
      newUser registerUserReq (T.PasswordSalt salt)
      &   runExceptT
      &   liftIO
      >>= either E.throw pure
    result <- liftIO $ HP.use pool (Session.statement user Q.insertUser)
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
  either (const (status status500)) json (signJwt tokenSigningKey token)

nextBytes
  :: MonadTrans t
  => MonadReader r m
  => MonadIO (t m)
  => T.HasCryptoStore r T.CryptoStore
  => Monad (t m)
  => Int
  -> t m ByteString
nextBytes byteCount = do
  tVar       <- lift $ asks (^. T.cryptoStore)
  currentGen <- liftIO $ readTVarIO tVar
  let (salt, nextGen) = genBytes byteCount currentGen
  liftIO $ atomically $ modifyTVar' tVar (const nextGen)
  pure salt

removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix ("api" : tail, queries) _ = (tail, queries)
removeApiPrefix paq                     _ = paq
