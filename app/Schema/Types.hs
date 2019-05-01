{-# LANGUAGE FunctionalDependencies #-}
module Schema.Types where

import           Control.Lens.TH                ( declareClassy )
import           Data.UUID.Types                ( UUID )
import           Data.Time.LocalTime            ( LocalTime )
import           Data.Text                      ( Text )



declareClassy [d|
  data Create t
      = Create
        { _createT :: t
        , _createLastUpdated :: LocalTime
        , _createCreatedAt :: LocalTime
        }

  data Update t
      = Update
        { _updateT :: t
        , _updateLastUpdated :: LocalTime
        }

  data Timestamped t
      = Timestamped
        { _timestampedT
        , _timestampedCreatedAt :: LocalTime
        , timestamptedLastUpdated :: LocalTime
        }

  data User
    = User
      { _userUuid :: UUID
      , _userUsername :: Text
      , _userHashedPassword :: Text
      , _userPublicKey :: Text
      }

  data Journal
    = Journal
      { _journalUuid :: UUID
      , _journalUserUuid :: UUID
      , _journalTitlteContent :: Text
      , _journalContent :: Text
      }

  |]
