module Types where

import           Protolude
import           Data.Aeson

newtype Username = Username Text
  deriving (Eq, Show, Generic)
instance ToJSON Username
instance FromJSON Username

newtype Base64Content = Base64Content Text
  deriving (Eq, Show, Generic)
instance ToJSON Base64Content
instance FromJSON Base64Content

newtype RawPassword = RawPassword Text
  deriving (Eq, Show, Generic)
instance ToJSON RawPassword
instance FromJSON RawPassword

newtype PublicKey = PublicKey Base64Content
  deriving (Eq, Show, Generic)
instance ToJSON PublicKey
instance FromJSON PublicKey

data RegisterUser
  = User
    { username :: Username
    , password :: RawPassword
    , publicKey :: PublicKey
    }
    deriving (Generic, Eq, Show)
instance ToJSON RegisterUser
instance FromJSON RegisterUser

newtype SessionToken = SessionToken Base64Content
  deriving (Eq, Show, Generic)
instance ToJSON SessionToken
instance FromJSON SessionToken
