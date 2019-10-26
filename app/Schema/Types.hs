{-# LANGUAGE FunctionalDependencies #-}
module Schema.Types where

import           Protolude                      ( Eq
                                                , Show
                                                )
import           Control.Lens.TH                ( declareClassy )
import           Types


-- This module represents types as they are written to and drawn from the DB
-- They should not be the same types that are permitted at the transport layer

declareClassy [d|
  data Create t
      = Create
        { createLastUpdated :: LastUpdated
        , createCreatedAt :: CreatedAt
        , createT :: t
        }
        deriving (Eq, Show)

  data Update t
      = Update
        { updateLastUpdated :: LastUpdated
        , updateT :: t
        }
        deriving (Eq, Show)

  data Timestamped t
      = Timestamped
        { timestampedCreatedAt :: CreatedAt
        , timestamptedLastUpdated :: LastUpdated
        , timestampedT :: t
        }
        deriving (Eq, Show)

  data User
    = User
      { userUserId :: UserId
      , userUsername :: Username
      , userHashedPassword :: HashedPassword
      , userPublicKey :: PublicKey
      }
      deriving (Eq, Show)

  data Journal
    = Journal
      { journalJournalId :: JournalId
      , journalUserId :: UserId
      , journalTitleContent :: TitleCiphertext
      , journalContent :: BodyCiphertext
      }
      deriving (Eq, Show)

  |]
