{-# LANGUAGE OverloadedStrings #-}
module Schema.Helpers where

import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax  ( PgColumnSchemaSyntax )
import           Database.Beam.Migrate.SQL      ( TableFieldSchema )
import           Database.Beam.Migrate.SQL.Tables
import           Data.Time.LocalTime            ( LocalTime )

lastUpdateField :: TableFieldSchema PgColumnSchemaSyntax LocalTime
lastUpdateField = field "last_update" timestamp (defaultTo_ now_) notNull
