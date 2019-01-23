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
                                                , flip
                                                , either
                                                , (<&>)
                                                , (&)
                                                , pure
                                                , liftIO
                                                , (<$>)
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
import           Control.Concurrent.STM         ( TVar
                                                , atomically
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
import           Types
import qualified Conf                          as Conf
import           Conf                           ( Environment(..) )
import qualified Data.Configurator             as C
import qualified Schema                        as Schema
import           System.Entropy                 ( getEntropy )
import           Crypto.Random.DRBG             ( HashDRBG
                                                , GenAutoReseed
                                                , newGenAutoReseed
                                                )
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
      sync              <- newTVarIO initialAppState
      putStrLn ("Initial state established, starting scotty app" :: String)
      let logger = Conf.logger env
      let runActionToIO m = runReaderT (runWebM m) sync
      scottyT 4000 runActionToIO $ app' conn logger
    )

data AppState =
  AppState
    { cryptoRandomGen :: GenAutoReseed HashDRBG HashDRBG
    , signingKey :: SigningKey
    }

{- A MonadTrans-like Monad for our application.
  Its kind is too refined however to allow this to have a MonadTrans instance
-}
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- | Lift a WebM a into another monad.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

generateInitialAppState :: Conf.Config -> ExceptT GenError IO AppState
generateInitialAppState conf = do
  initialEntropy <- lift $ getEntropy 256
  gen            <- ExceptT $ return
    (newGenAutoReseed initialEntropy (2 ^ 48) :: Either
        GenError
        (GenAutoReseed HashDRBG HashDRBG)
    )
  return $ AppState gen (Conf.signingKey conf)

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
    loginUser   <- jsonData :: ActionT' LoginUser
    desiredUser <- runStatement' (loginUser ^. username) Q.findUserByUsername
    case desiredUser of
      Right (Just user) -> do
        let correctPassword  = HashedPassword $ Schema._userHashedPassword user
        let providedPassword = loginUser ^. rawPassword
        let passwordVerificationResult =
              verifyPassword correctPassword providedPassword
        case passwordVerificationResult of
          Argon2.Argon2Ok -> respondWithAuthToken user
          _               -> status status400
      _ -> status status400

  post "/register" $ do
    registerUser <- jsonData :: ActionT' RegisterUser
    salt         <- nextBytes 16
    user         <-
      newUser registerUser (PasswordSalt salt)
      &   runExceptT
      &   liftIO
      >>= either E.throw pure
    result <- liftIO $ HP.use pool (Session.statement user Q.insertUser)
    case result of
      Left err -> do
        putStrLn (show err :: ByteString)
        status status400
      Right _ -> respondWithAuthToken user

respondWithAuthToken :: Schema.User -> ActionT' ()
respondWithAuthToken user = do
  token           <- liftIO $ createAccessToken user
  tokenSigningKey <- webM (gets signingKey)
  either (const (status status500)) json (signJwt tokenSigningKey token)

nextBytes :: Int -> ActionT' ByteString
nextBytes byteCount = do
  (salt, nextGen) <- webM (gets cryptoRandomGen <&> genBytes byteCount)
  webM $ modify $ \st -> st { cryptoRandomGen = nextGen }
  pure salt

removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix ("api" : tail, queries) _ = (tail, queries)
removeApiPrefix paq                     _ = paq
