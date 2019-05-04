{-# LANGUAGE FunctionalDependencies #-}
module Types where

import           Protolude                      ( Eq
                                                , Show
                                                , Generic
                                                )

import           Data.UUID                      ( UUID )
import           Control.Lens
import           Data.ByteString                ( ByteString )
import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Control.Concurrent.STM         ( TVar )
import           Crypto.Random.DRBG             ( HashDRBG
                                                , GenAutoReseed
                                                )
newtype Username = Username { usernameText :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''Username

newtype Base64Content = Base64Content { base64ContentText :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''Base64Content

newtype RawPassword = RawPassword { rawPasswordText :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''RawPassword

newtype PublicKey = PublicKey { publicKeyBase64Content :: Base64Content }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''PublicKey

newtype CreatedAt = CreatedAt { createdAtUTCTime :: UTCTime }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''CreatedAt

newtype LastUpdated = LastUpdated { lastUpdatedUTCTime :: UTCTime }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''LastUpdated

newtype EncryptedMessage = EncryptedMessage
  { encryptedMessageText :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''EncryptedMessage

newtype TitleCiphertext = TitleCiphertext { titleCiphertextEncryptedMessage :: EncryptedMessage }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''TitleCiphertext

newtype BodyCiphertext = BodyCiphertext { bodyCiphertextEncryptedMessage :: EncryptedMessage }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''BodyCiphertext

newtype UserId = UserId { userIdUUID :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''UserId

newtype JournalId = JournalId { journalIdUUID :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
makeFields ''JournalId

newtype HashedPassword = HashedPassword { hashedPasswordText :: Text }
  deriving (Eq, Generic, Show)
makeFields ''HashedPassword

newtype SigningKey = SigningKey { signingKeyByteString :: ByteString }
  deriving (Eq, Show)
makeFields ''SigningKey

newtype PasswordSalt = PasswordSalt { passwordSaltByteString :: ByteString }
makeFields ''PasswordSalt

type CryptoStore = TVar (GenAutoReseed HashDRBG HashDRBG)

data AppState = AppState
  { appStateCryptoStore :: CryptoStore
  , appStateSigningKey :: SigningKey
  }
  deriving (Generic)
makeFields ''AppState