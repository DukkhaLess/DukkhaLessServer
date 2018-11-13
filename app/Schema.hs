{-# LANGUAGE OverloadedStrings #-}
module Schema
  ( module V0001
  , migrationStep
  , dukkhalessDb
  , runMigrations
  , insertUser
  , findUserbyUsername
  )
where

import           Protolude
import           Database.Beam
import           Database.Beam.Migrate.Types    ( CheckedDatabaseSettings
                                                , MigrationSteps
                                                , migrationStep
                                                , evaluateDatabase
                                                , unCheckDatabase
                                                )
import           Database.Beam.Migrate.Simple   ( bringUpToDate )
import           Database.Beam.Postgres         ( PgCommandSyntax
                                                , Connection
                                                , Pg
                                                )
import           Database.Beam.Postgres.Migrate ( migrationBackend )
import           Schema.V0001            hiding ( migration, DukkhalessCons )
import qualified Schema.V0001                  as V0001
import qualified Types                         as T

data DukkhalessDb f
  = DukkhalessDb
    { _dukkalessUsers :: f (TableEntity UserT)
    }
    deriving Generic

instance Database be DukkhalessDb

dukkhalessDb :: DatabaseSettings be DukkhalessDb
dukkhalessDb = unCheckDatabase (evaluateDatabase migrations)

migrations
  :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings be DukkhalessDb)
migrations = migrationStep "Initial commit" (V0001.migration DukkhalessDb)

runMigrations :: Connection -> IO ()
runMigrations conn =
  void $ withDatabase conn $ bringUpToDate migrationBackend migrations

insertUser :: User -> Connection -> IO ()
insertUser u conn = withDatabase conn cmd
 where
  cmd :: Pg ()
  cmd = runInsert $ insert (_dukkalessUsers dukkhalessDb) $ insertValues [u]

findUserbyUsername :: T.Username -> Connection -> IO (Maybe User)
findUserbyUsername (T.Username name) conn = withDatabase conn cmd
 where
  cmd :: Pg (Maybe User)
  cmd = runSelectReturningOne $ select $ filter_
    (\u -> _userUsername u ==. val_ name)
    (all_ (_dukkalessUsers dukkhalessDb))
