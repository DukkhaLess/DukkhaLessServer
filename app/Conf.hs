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
                                                , Ord
                                                , Bounded
                                                , Eq
                                                , FilePath
                                                , (<&>)
                                                , Int
                                                , Integer
                                                , fromInteger
                                                )
import           Data.Word                      ( Word16 )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Data.Text.Lazy                 ( toLower
                                                , pack
                                                , Text
                                                )
import           Data.ByteString                ( ByteString )
import           Hasql.Connection              as HC
import           Hasql.Pool                    as HP
import qualified Types                         as T
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
  deriving (Read, Show, Ord, Bounded, Eq)

newtype MigrationsPath
  = MigrationsPath FilePath
  deriving (Read, Show, Ord, Eq)

logger :: Environment -> Middleware
logger Production = logStdout
logger _          = logStdoutDev

confFileName :: Environment -> Text
confFileName = toLower . pack . flip (++) ".conf" . show

data DatabaseUser
  = DatabaseUser
    { username :: ByteString
    , password :: ByteString
    , schema :: ByteString
    }

data PoolSettings
  = PoolSettings
    { poolSize :: Int
    , timeoutMs :: Integer
    }

data DatabaseConfig
  = DatabaseConfig
    { applicationAccount :: DatabaseUser
    , postgresPort :: Word16
    , postgresHost :: ByteString
    , migrationsPath :: MigrationsPath
    , poolSettings :: PoolSettings
    }

newtype HttpConfig
  = HttpConfig
    { domain :: ByteString
    }

data Config
  = Config
    { databaseConfig :: DatabaseConfig
    , signingKey :: T.SigningKey
    , httpSettings :: HttpConfig
    }

makeConfig :: C.Config -> IO (Maybe Config)
makeConfig conf = runMaybeT $ do
  dbConf <- do
    app <- do
      name     <- MaybeT $ C.lookup conf "postgres.app.username"
      pass     <- MaybeT $ C.lookup conf "postgres.app.password"
      database <- MaybeT $ C.lookup conf "postgres.app.database"
      return $ DatabaseUser name pass database
    hostname <- MaybeT $ C.lookup conf "postgres.host"
    port     <- MaybeT $ C.lookup conf "postgres.port"
    pool     <- do
      size    <- MaybeT $ C.lookup conf "postgres.pool.maxPool"
      timeout <- MaybeT $ C.lookup conf "postgres.pool.timeoutMs"
      return $ PoolSettings size timeout
    migrations <-
      (MaybeT $ C.lookup conf "postgress.migrationsPath") <&> MigrationsPath
    return $ DatabaseConfig app port hostname migrations pool
  httpConfig <- do
    dmn <- MaybeT $ C.lookup conf "http.domain"
    return $ HttpConfig dmn
  sk <- MaybeT $ C.lookup conf "crypto.signingKey"
  return $ Config dbConf (T.SigningKey sk) httpConfig

connectInfo :: DatabaseConfig -> (DatabaseConfig -> DatabaseUser) -> HP.Settings
connectInfo dbConf getUser =
  (poolSize pool, fromInteger $ timeoutMs pool, connectionSettings)
 where
  user               = getUser dbConf
  pool               = poolSettings dbConf
  connectionSettings = HC.settings (postgresHost dbConf)
                                   (postgresPort dbConf)
                                   (username user)
                                   (password user)
                                   (schema user)
