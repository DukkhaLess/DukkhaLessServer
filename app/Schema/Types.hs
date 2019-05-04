{-# LANGUAGE FunctionalDependencies #-}
module Schema.Types where

import           Control.Lens.TH                ( declareClassy )
import           Data.Time.Clock                ( UTCTime )
import Types



declareClassy [d|
  data Create t
      = Create
        { createLastUpdated :: UTCTime
        , createCreatedAt :: UTCTime
        , createT :: t
        }

  data Update t
      = Update
        { updateLastUpdated :: UTCTime
        , updateT :: t
        }

  data Timestamped t
      = Timestamped
        { timestampedCreatedAt :: UTCTime
        , timestamptedLastUpdated :: UTCTime
        , timestampedT :: t
        }

  data User
    = User
      { userUuid :: UserId
      , userUsername :: Username
      , userHashedPassword :: HashedPassword
      , userPublicKey :: PublicKey
      }

  data Journal
    = Journal
      { journalUuid :: JournalId
      , journalUserUuid :: UserId
      , journalTitleContent :: TitleCiphertext
      , journalContent :: BodyCiphertext
      }

  |]
