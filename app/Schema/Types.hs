{-# LANGUAGE FunctionalDependencies #-}
module Schema.Types where

import           Protolude                      ( Eq
                                                , Show
                                                )
import           Control.Lens.TH                ( declareClassy )
import           Data.Time.Clock                ( UTCTime )
import           Types


-- This module represents types as they are written to and drawn from the DB
-- They should not be the same types that are permitted at the transport layer

declareClassy [d|
  data Create t
      = Create
        { createLastUpdated :: UTCTime
        , createCreatedAt :: UTCTime
        , createT :: t
        }
        deriving (Eq, Show)

  data Update t
      = Update
        { updateLastUpdated :: UTCTime
        , updateT :: t
        }
        deriving (Eq, Show)

  data Timestamped t
      = Timestamped
        { timestampedCreatedAt :: UTCTime
        , timestamptedLastUpdated :: UTCTime
        , timestampedT :: t
        }
        deriving (Eq, Show)

  data User
    = User
      { userUuid :: UserId
      , userUsername :: Username
      , userHashedPassword :: HashedPassword
      , userPublicKey :: PublicKey
      }
      deriving (Eq, Show)

  data Journal
    = Journal
      { journalUuid :: JournalId
      , journalUserUuid :: UserId
      , journalTitleContent :: TitleCiphertext
      , journalContent :: BodyCiphertext
      }
      deriving (Eq, Show)

  |]
