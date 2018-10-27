{-# LANGUAGE OverloadedStrings #-}
module Conf where

import           Protolude                      ( show
                                                , flip
                                                , (++)
                                                , Read
                                                , Show
                                                , Maybe
                                                , (.)
                                                , IO
                                                , return
                                                , ($)
                                                )
import           Data.Word                      ( Word16 )
import           Database.Beam.Postgres         ( ConnectInfo(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Data.String                    ( String )
import           Data.Text.Lazy                 ( toLower
                                                , pack
                                                , Text
                                                )
import           Data.ByteString                ( ByteString )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout
                                                , logStdoutDev
                                                )
import           Network.Wai                    ( Middleware )
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C

data Environment
  = Development
  | Production
  deriving (Read, Show)

logger :: Environment -> Middleware
logger Production = logStdout
logger _          = logStdoutDev

confFileName :: Environment -> Text
confFileName = toLower . pack . flip (++) ".conf" . show

data DatabaseUser
  = DatabaseUser
    { username :: String
    , password :: String
    , schema :: String
    }

data DatabaseConfig
  = DatabaseConfig
    { rootAccount :: DatabaseUser
    , applicationAccount :: DatabaseUser
    , postgresPort :: Word16
    , postgresHost :: String
    }

data HttpConfig
  = HttpConfig
    { domain :: ByteString
    }

data Config
  = Config
    { databaseConfig :: DatabaseConfig
    , signingKey :: Text
    , httpSettings :: HttpConfig
    }

connectInfo :: DatabaseConfig -> (DatabaseConfig -> DatabaseUser) -> ConnectInfo
connectInfo conf usrFn =
  ConnectInfo
    (postgresHost conf)
    (postgresPort conf)
    (username user)
    (password user)
    (schema user)
  where user = usrFn conf


makeConfig :: C.Config -> IO (Maybe Config)
makeConfig conf = runMaybeT $ do
  dbConf <- do
    root <- do
      name     <- MaybeT $ C.lookup conf "postgres.root.username"
      pass     <- MaybeT $ C.lookup conf "postgres.root.password"
      database <- MaybeT $ C.lookup conf "postgres.root.database"
      return $ Conf.DatabaseUser name pass database
    app <- do
      name     <- MaybeT $ C.lookup conf "postgres.app.username"
      pass     <- MaybeT $ C.lookup conf "postgres.app.password"
      database <- MaybeT $ C.lookup conf "postgres.app.database"
      return $ Conf.DatabaseUser name pass database
    hostname <- MaybeT $ C.lookup conf "postgres.host"
    port     <- MaybeT $ C.lookup conf "postgres.port"
    return $ Conf.DatabaseConfig root app port hostname
  httpConfig <- do
    dmn <- MaybeT $ C.lookup conf "http.domain"
    return $ HttpConfig dmn
  sk <- MaybeT $ C.lookup conf "crypto.signingKey"
  return $ Conf.Config dbConf sk httpConfig

