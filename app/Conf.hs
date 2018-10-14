{-# LANGUAGE OverloadedStrings #-}
module Conf where

import           Protolude                      ( show
                                                , flip
                                                , (++)
                                                , Read
                                                , Show
                                                , Int
                                                , (.)
                                                , IO
                                                , return
                                                , ($)
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Data.Text.Lazy                 ( toLower
                                                , pack
                                                , Text
                                                )
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
    { username :: Text
    , password :: Text
    , schema :: Text
    }

data DatabaseConfig
  = DatabaseConfig
    { rootAccount :: DatabaseUser
    , applicationAccount :: DatabaseUser
    , postgresPort :: Int
    , postgresHost :: Text
    }

data Config
  = Config
    { databaseConfig :: DatabaseConfig
    , signingKey :: Text
    }


makeConfig :: C.Config -> MaybeT IO Config
makeConfig conf = do
  dbConf <- do
    root <- do
      name     <- MaybeT $ C.lookup conf "postgres.root.username"
      pass <- MaybeT $ C.lookup conf "postgres.root.password"
      database <- MaybeT $ C.lookup conf "postgres.root.database"
      return $ Conf.DatabaseUser name pass database
    app <- do
      name     <- MaybeT $ C.lookup conf "postgres.app.username"
      pass <- MaybeT $ C.lookup conf "postgres.app.password"
      database <- MaybeT $ C.lookup conf "postgres.app.database"
      return $ Conf.DatabaseUser name pass database
    hostname <- MaybeT $ C.lookup conf "postgres.host"
    port     <- MaybeT $ C.lookup conf "postgres.port"
    return $ Conf.DatabaseConfig root app port hostname
  sk <- MaybeT $ C.lookup conf "crypto.signingKey"
  return $ Conf.Config dbConf sk

