module Conf where

import           Protolude                      ( show
                                                , flip
                                                , (++)
                                                , Read
                                                , Show
                                                , Int
                                                , (.)
                                                )
import           Data.Text.Lazy                 ( toLower
                                                , pack
                                                , Text
                                                )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout
                                                , logStdoutDev
                                                )
import           Network.Wai                    ( Middleware )

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
