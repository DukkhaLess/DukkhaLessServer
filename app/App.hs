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
import           Data.Default                   ( def )
import           Data.Text.Lazy                 ( unpack
                                                , fromStrict
                                                )
import qualified Database.Beam.Postgres        as Pg
import           Domain                         ( newUser )
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , readTVarIO
                                                , modifyTVar'
                                                , newTVarIO
                                                )
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
import           Web.Scotty.Trans
import           Types
import qualified Conf                          as Conf
import           Conf                           ( Environment(..) )
import qualified Data.Configurator             as C
import           Schema                         ( runMigrations )
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
    ( Pg.connect
    $ Conf.connectInfo (Conf.databaseConfig conf) Conf.applicationAccount
    )
    Pg.close
    (\conn -> do
      runMigrations conn
      let logger =
            (case env of
              Production  -> logStdout
              Development -> logStdoutDev
            )
      eitherErrAppState <- runExceptT generateInitialAppState
      initialAppState   <- either E.throwIO return eitherErrAppState
      sync              <- newTVarIO initialAppState
      let runActionToIO m = runReaderT (runWebM m) sync
      scottyT 4000 runActionToIO $ app' conn logger
    )

newtype AppState =
  AppState
    { cryptoRandomGen :: GenAutoReseed HashDRBG HashDRBG
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

generateInitialAppState :: ExceptT GenError IO AppState
generateInitialAppState = do
  initialEntropy <- lift $ getEntropy 256
  gen            <- ExceptT $ return
    (newGenAutoReseed initialEntropy (2 ^ 48) :: Either
        GenError
        (GenAutoReseed HashDRBG HashDRBG)
    )
  return $ AppState gen

type ActionT' = ActionT LT.Text WebM

app' :: Pg.Connection -> Middleware -> ScottyT LT.Text WebM ()
app' _ logger = do
  middleware $ rewritePureWithQueries removeApiPrefix
  middleware logger
  middleware $ gzip def
  post "/login" $ do
    loginUser <- jsonData :: ActionT' LoginUser
    text $ fromStrict $ loginUser ^. (username . _text)
  post "/register" $ do
    registerUser    <- jsonData :: ActionT' RegisterUser
    (salt, nextGen) <- webM (gets cryptoRandomGen <&> genBytes 16)
    _               <-
      newUser registerUser (PasswordSalt salt)
      &   runExceptT
      &   liftIO
      >>= either E.throw pure
    webM $ modify $ \st -> st { cryptoRandomGen = nextGen }
    text $ fromStrict $ registerUser ^. (username . _text)

removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix ("api" : tail, queries) _ = (tail, queries)
removeApiPrefix paq                     _ = paq
