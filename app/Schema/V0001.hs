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
import           Data.Time.LocalTime            ( LocalTime )
import           Database.Beam.Migrate.Types
                                         hiding ( migrateScript )
import           Database.Beam.Migrate.SQL.Tables
import           Data.Text                      ( Text )
import Schema.Helpers

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

type DukkhalessCons be db = CheckedDatabaseEntity
                                      be db (TableEntity UserT)
                                    -> CheckedDatabaseSettings be db

migration
  :: DukkhalessCons be db
  -> ()
  -> Migration PgCommandSyntax (CheckedDatabaseSettings be db)
migration db _ = db <$> createTable
  "users"
  (User (field "user_uuid" uuid notNull unique)
        (field "username" (varchar (Just 50)) notNull unique)
        (field "hashed_password" (varchar (Just 256)) notNull)
        (field "public_key" (varchar (Just 512)) notNull unique)
        lastUpdateField
  )
