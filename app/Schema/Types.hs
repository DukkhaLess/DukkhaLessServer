{-# LANGUAGE FunctionalDependencies #-}
module Schema.Types where

import           Control.Lens.TH                ( declareClassy )
import           Data.UUID.Types                ( UUID )
import           Data.Time.Clock                ( UTCTime(..) )
import           Data.Text                      ( Text )



declareClassy [d|
  data Create t
      = Create
        { createT :: t
        , createLastUpdated :: UTCTime
        , createCreatedAt :: UTCTime
        }

  data Update t
      = Update
        { updateT :: t
        , updateLastUpdated :: UTCTime
        }

  data Timestamped t
      = Timestamped
        { timestampedT
        , timestampedCreatedAt :: UTCTime
        , timestamptedLastUpdated :: UTCTime
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
