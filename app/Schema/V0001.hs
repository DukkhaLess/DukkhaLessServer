{-# LANGUAGE OverloadedStrings #-}
module Schema.V0001 where

import           Protolude                      ( Show
                                                , Eq
                                                , Identity
                                                , (.)
                                                , (<$>)
                                                , Maybe(..)
                                                )
import           Data.UUID.Types                ( UUID )
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax  ( PgColumnSchemaSyntax )
import           Data.Time.LocalTime            ( LocalTime )
import           Database.Beam.Migrate.Types
                                         hiding ( migrateScript )
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.SQL      ( TableFieldSchema )
import           Data.Text                      ( Text )

data UserT f
  = User
    { _userUuid :: Columnar f UUID
    , _userUsername :: Columnar f Text
    , _userHashedPassword :: Columnar f Text
    , _userPublicKey :: Columnar f Text
    , _userLastUpdate :: Columnar f LocalTime
    }
    deriving (Generic)
instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f UUID) deriving Generic
  primaryKey = UserId . _userUuid

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

type UserId = PrimaryKey UserT Identity
instance Beamable (PrimaryKey UserT)

data DukkhalessDb f
  = DukkhalessDb
    { _dukkalessUsers :: f (TableEntity UserT)
    }
    deriving Generic

instance Database be DukkhalessDb

lastUpdateField :: TableFieldSchema PgColumnSchemaSyntax LocalTime
lastUpdateField = field "last_update" timestamp (defaultTo_ now_) notNull

migration
  :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings be DukkhalessDb)
migration () = DukkhalessDb <$> createTable
  "users"
  (User (field "user_uuid" uuid notNull unique)
        (field "username" (varchar (Just 50)) notNull unique)
        (field "hashed_password" (varchar (Just 256)) notNull)
        (field "public_key" (varchar (Just 512)) notNull unique)
        lastUpdateField
  )
