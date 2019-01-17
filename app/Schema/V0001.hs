{-# LANGUAGE OverloadedStrings #-}
module Schema.V0001 where

{-- import           Protolude                      ( Show
                                                , Eq
                                                , Identity
                                                , (.)
                                                , (<$>)
                                                , Maybe(..)
                                                )
                                                --}
import           Data.UUID.Types                ( UUID )
import           Data.Time.LocalTime            ( LocalTime )
import           Data.Text                      ( Text )

data User
  = User
    { _userUuid :: UUID
    , _userUsername :: Text
    , _userHashedPassword :: Text
    , _userPublicKey :: Text
    , _userLastUpdated :: LocalTime
    , _userCreatedAt :: LocalTime
    }

{--
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
--}
