{-# LANGUAGE OverloadedStrings #-}
module Schema where

import           Database.Beam                  ( DatabaseSettings )
import           Database.Beam.Migrate.Types    ( CheckedDatabaseSettings
                                                , MigrationSteps
                                                , unCheckDatabase
                                                , evaluateDatabase
                                                , migrationStep
                                                )
import           Database.Beam.Postgres         ( Postgres
                                                , PgCommandSyntax
                                                )
import           Schema.V0001            hiding ( migration )
import qualified Schema.V0001                  as V0001
                                                ( migration )

dukkhalessDb :: DatabaseSettings Postgres DukkhalessDb
dukkhalessDb = unCheckDatabase (evaluateDatabase migration)

migration
  :: MigrationSteps
       PgCommandSyntax
       ()
       (CheckedDatabaseSettings Postgres DukkhalessDb)
migration = migrationStep "Initial commit" V0001.migration
