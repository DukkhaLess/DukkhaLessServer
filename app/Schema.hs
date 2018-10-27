{-# LANGUAGE OverloadedStrings #-}
module Schema where

import           Protolude
import           Database.Beam                  ( DatabaseSettings
                                                , withDatabase
                                                )
import           Database.Beam.Migrate.Types    ( CheckedDatabaseSettings
                                                , MigrationSteps
                                                , migrationStep
                                                , evaluateDatabase
                                                , unCheckDatabase
                                                )
import           Database.Beam.Migrate.Simple   ( bringUpToDate )
import           Database.Beam.Postgres         ( Postgres
                                                , PgCommandSyntax
                                                , Connection
                                                )
import           Database.Beam.Postgres.Migrate ( migrationBackend )
import           Schema.V0001            hiding ( migration )
import qualified Schema.V0001                  as V0001
                                                ( migration )

dukkhalessDb :: DatabaseSettings Postgres DukkhalessDb
dukkhalessDb = unCheckDatabase (evaluateDatabase migrations)

migrations
  :: MigrationSteps
       PgCommandSyntax
       ()
       (CheckedDatabaseSettings Postgres DukkhalessDb)
migrations = migrationStep "Initial commit" V0001.migration

runMigrations :: Connection -> IO ()
runMigrations conn =
  void $ withDatabase conn $ bringUpToDate migrationBackend migrations
