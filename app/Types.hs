module Types where

import           Protolude                      ( Eq
                                                , Show
                                                , Generic
                                                )

import           Data.UUID                      ( UUID )
import           Control.Lens
import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )

declareClassy [d|
  newtype Username = Username { usernameText :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
  newtype Base64Content = Base64Content { base64ContentText :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype RawPassword = RawPassword { rawPasswordText :: Text }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype PublicKey = PublicKey { publicKeyBase64Content :: Base64Content }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype CreatedAt = CreatedAt { createdAtUTCTime :: UTCTime }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype LastUpdated = LastUpdated { lastUpdatedUTCTime :: UTCTime }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype EncryptedMessage = EncryptedMessage
    { encryptedMessageValue :: Value }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype TitleCiphertext = TitleCiphertext { titleCiphertextEncryptedMessage :: EncryptedMessage }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype BodyCiphertext = BodyCiphertext { bodyCiphertextEncrypedMessage :: EncryptedMessage }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype UserId = UserId { userIdUUID :: UUID }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype JournalId = JournalId { journalIdUUID :: UUID }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype TokenId = TokenId { tokenIdUUID :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype HashedPassword = HashedPassword { hashedPasswordText :: Text }

 |]
