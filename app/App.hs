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
                                                )
import qualified Control.Exception             as E
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )

import           Control.Monad.Trans.Except     ( ExceptT(..) )
import           Control.Monad.Trans            ( lift )
import           Data.Default                   ( def )
import           Data.Text.Lazy                 ( unpack
                                                , fromStrict
                                                )
import qualified Database.Beam.Postgres        as Pg
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , readTVarIO
                                                , modifyTVar'
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
import           Web.Scotty
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
    (\conn -> app'
      conn
      (case env of
        Production  -> logStdout
        Development -> logStdoutDev
      )
    )

newtype AppState =
  AppState
    { cryptoRandomGen :: GenAutoReseed HashDRBG HashDRBG
    }

newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Some helpers to make this feel more like a state monad.
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

generateInitialAppState :: ExceptT GenError IO AppState
generateInitialAppState = do
  initialEntropy <- lift $ getEntropy 256
  gen            <-
    ExceptT
    $ return
    $ (newGenAutoReseed initialEntropy (2 ^ 48) :: Either
          GenError
          (GenAutoReseed HashDRBG HashDRBG)
      )
  return $ AppState gen

app' :: Pg.Connection -> Middleware -> IO ()
app' conn logger = do
  runMigrations conn
  scotty 4000 $ do
    middleware $ rewritePureWithQueries removeApiPrefix
    middleware logger
    middleware $ gzip def
    get "/:word" $ html "Hi"
    post "/login" $ do
      loginUser <- jsonData :: ActionM LoginUser
      text $ fromStrict $ loginUser ^. (username . _text)
    post "/register" $ do
      registerUser <- jsonData :: ActionM RegisterUser
      text $ fromStrict $ registerUser ^. (username . _text)

removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix ("api" : tail, queries) _ = (tail, queries)
removeApiPrefix paq                     _ = paq

