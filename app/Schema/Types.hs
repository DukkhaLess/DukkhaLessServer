{-# LANGUAGE FunctionalDependencies #-}
module Schema.Types where

import           Control.Lens.TH                ( declareClassy )
import           Data.UUID.Types                ( UUID )
import           Data.Time.Clock                ( UTCTime(..) )
import           Data.Text                      ( Text )



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
      { userUuid :: UUID
      , userUsername :: Text
      , userHashedPassword :: Text
      , userPublicKey :: Text
      }

  data Journal
    = Journal
      { journalUuid :: UUID
      , journalUserUuid :: UUID
      , journalTitleContent :: Text
      , journalContent :: Text
      }

  |]
