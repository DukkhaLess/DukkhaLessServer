{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module App where

import           Protolude                      ( IO
                                                , ($)
                                                , (.)
                                                , (>>=)
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
                                                , Show
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
import           Domain                         ( newUser
                                                , createAccessToken
                                                )
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , readTVarIO
                                                , modifyTVar'
                                                , newTVarIO
                                                )
import           Hasql.Connection              as HC
import           Hasql.Pool                    as HP
import           Network.Wai                    ( Middleware )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev
                                                , logStdout
                                                )
import           Network.Wai.Middleware.Rewrite ( PathsAndQueries
                                                , rewritePureWithQueries
                                                )
import           Network.Wai.Middleware.Gzip    ( gzip )
import           Network.HTTP.Types.Header      ( RequestHeaders )
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans
import           Web.Scotty.Internal.Types      ( ActionError )
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

app :: Conf.Environment -> IO ()
app env = void $ runMaybeT $ do
  conf <- MaybeT
    (C.load [C.Required $ unpack $ Conf.confFileName env] >>= Conf.makeConfig)
  lift $ E.bracket
    ( HP.acquire
    $ Conf.connectInfo (Conf.databaseConfig conf) Conf.applicationAccount
    )
    HP.release
    (\conn -> do
      migrationResult <- runMigrations
        (Conf.migrationsPath . Conf.databaseConfig conf)
        conn
      _                 <- either (fail . show) pure migrationResult
      eitherErrAppState <- runExceptT (generateInitialAppState conf)
      initialAppState   <- either E.throwIO return eitherErrAppState
      sync              <- newTVarIO initialAppState
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

app' :: HP.Pool -> Middleware -> ScottyT LT.Text WebM ()
app' conn logger = do
  middleware $ rewritePureWithQueries removeApiPrefix
  middleware logger
  middleware $ gzip def
  post "/login" $ do
    loginUser   <- jsonData :: ActionT' LoginUser
    desiredUser <- liftIO
      $ Schema.findUserbyUsername (loginUser ^. username) conn
    case desiredUser of
      Just user -> do
        let correctPassword  = HashedPassword $ Schema._userHashedPassword user
        let providedPassword = loginUser ^. rawPassword
        let passwordVerificationResult =
              verifyPassword correctPassword providedPassword
        case passwordVerificationResult of
          Argon2.Argon2Ok -> respondWithAuthToken user
          _               -> status status400
      Nothing -> status status400

  post "/register" $ do
    registerUser <- jsonData :: ActionT' RegisterUser
    salt         <- nextBytes 16
    user         <-
      newUser registerUser (PasswordSalt salt)
      &   runExceptT
      &   liftIO
      >>= either E.throw pure
    liftIO $ Schema.insertUser user conn
    respondWithAuthToken user

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
