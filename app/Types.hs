{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Protolude
import           Control.Lens.Combinators
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

data RegisterUser = RegisterUser Username RawPassword PublicKey
    deriving (Generic, Eq, Show)
instance FromJSON RegisterUser where
  parseJSON = withObject "loginUser" $ \o ->
    RegisterUser <$> o .: "username" <*> o .: "password" <*> o .: "publicKey"

newtype SessionToken = SessionToken Base64Content
  deriving (Eq, Show, Generic)
instance ToJSON SessionToken
instance FromJSON SessionToken

data LoginUser = LoginUser Username RawPassword
  deriving (Eq, Show, Generic)
instance FromJSON LoginUser where
  parseJSON = withObject "loginUser" $ \o ->
    LoginUser <$> o .: "username" <*> o .: "password"

class HasRawPassword a where rawPassword :: Lens' a RawPassword
instance HasRawPassword RawPassword where rawPassword = identity
instance HasRawPassword LoginUser where rawPassword = lens (\(LoginUser _ p) -> p) (\(LoginUser u _) p -> LoginUser u p)

class HasUsername a where username :: Lens' a Username
instance HasUsername Username where username = identity
instance HasUsername LoginUser where username = lens (\(LoginUser u _) -> u) (\(LoginUser _ p) u -> LoginUser u p)
