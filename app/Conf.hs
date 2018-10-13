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

data Config
  = Config
    { postgresUsername :: Text
    , postgresPassword :: Text
    , postgresHost :: Text
    , postgresPort :: Int
    , postgresDatabase :: Text
    , signingKeyString :: Text
    }
