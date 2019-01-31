{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Protolude                      ( Eq
                                                , Show
                                                , ($)
                                                , (<*>)
                                                , (<$>)
                                                , Generic
                                                , (<>)
                                                , Maybe
                                                )
import           Control.Lens.Combinators
import           Control.Lens.TH                (makeClassy)
import           Data.ByteString                ( ByteString )
import           Data.Aeson
import           Data.Aeson.Types               ( typeMismatch )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.Time.Clock                ( UTCTime )
import           Control.Concurrent.STM         ( TVar )
import           Crypto.Random.DRBG             ( HashDRBG
                                                , GenAutoReseed
                                                )

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

  data RegisterUser = RegisterUser
    { registerUserUsername :: Username
    , registerUserRawPassword :: RawPassword
    , registerUserPublicKey :: PublicKey
    }
    deriving (Generic, Eq, Show)

  data LoginUser = LoginUser { loginUserUsername :: Username, loginUserRawPassword :: RawPassword }
    deriving (Eq, Show, Generic)

  newtype HashedPassword = HashedPassword { hashedPasswordText :: Text }

  newtype PasswordSalt = PasswordSalt { passwordSaltByteString :: ByteString }

  newtype TokenId = TokenId { tokenIdUUID :: UUID }
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

  data JournalEntry = JournalEntry
    { journalEntryJournalId :: JournalId
    , journalEntryCreatedAt :: CreatedAt
    , journalEntryUserId :: UserId
    , journalEntryLastUpdated :: LastUpdated
    , journalEntryTitleCiphertext :: TitleCiphertext
    , journalEntryBodyCiphertext :: BodyCiphertext
    }
    deriving (Eq, Show, Generic)

  data UpdateJournalEntry = UpdateJournalEntry
    { updateJournalEntryBodyCiphertext :: BodyCiphertext
    , updateJournalEntryTitleCiphertext :: TitleCiphertext
    , updateJournalEntryJournalId :: Maybe JournalId
    }
    deriving (Eq, Show, Generic)

  newtype UserId = UserId { userIdUUID :: UUID }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype JournalId = JournalId { journalIdUUID :: UUID }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  newtype Expiry = Expiry { expiryUTCTime :: UTCTime }
      deriving (Eq, Show, Generic, ToJSON, FromJSON)

  data AccessToken = AccessToken
    { accessTokenTokenId ::  TokenId
    , accessTokenUserId :: UserId
    , accessTokenExpiry :: Expiry
    }
    deriving (Eq, Show, Generic)

  newtype SigningKey = SigningKey { signingKeyByteString :: ByteString }
    deriving (Eq, Show)

  newtype SessionToken = SessionToken { sessionTokenBase64Content :: Base64Content }
    deriving (Eq, Show, Generic)

  |]

data AppState = AppState
  { _appStateCryptoRandomGen :: TVar (GenAutoReseed HashDRBG HashDRBG)
  , _appStateSigningKey :: SigningKey
  }
  deriving (Generic)
makeClassy ''AppState


instance FromJSON RegisterUser where
  parseJSON = withObject "loginUser" $ \o ->
    RegisterUser <$> o .: "username" <*> o .: "password" <*> o .: "publicKey"

instance FromJSON LoginUser where
  parseJSON = withObject "loginUser"
    $ \o -> LoginUser <$> o .: "username" <*> o .: "password"

instance ToJSON AccessToken where
  toJSON (AccessToken ti ui e) = object ["jti" .= ti, "sub" .= ui, "exp" .= e]
  toEncoding (AccessToken ti ui e) =
    pairs ("jti" .= ti <> "sub" .= ui <> "exp" .= e)
instance FromJSON AccessToken where
  parseJSON (Object v) =
    AccessToken <$> v .: "jti" <*> v .: "sub" <*> v .: "exp"
  parseJSON invalid = typeMismatch "AccessToken" invalid

instance ToJSON SessionToken where
  toJSON (SessionToken content) = object ["sessionToken" .= content]
  toEncoding (SessionToken content) = pairs ("sessionToken" .= content)

instance FromJSON SessionToken where
  parseJSON (Object v) = SessionToken <$> v .: "sessionToken"
  parseJSON invalid    = typeMismatch "SessionToken" invalid
