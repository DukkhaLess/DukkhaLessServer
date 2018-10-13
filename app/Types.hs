{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Protolude                      ( Eq
                                                , Show
                                                , ($)
                                                , (<*>)
                                                , (<$>)
                                                , identity
                                                , Generic
                                                , (.)
                                                )
import           Control.Lens.Combinators
import           Data.Aeson
import           Data.Text.Lazy                 ( Text )

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

class HasText a where _text :: Lens' a Text
instance HasText Text where _text = identity
instance HasText RawPassword where _text = lens (\(RawPassword t) -> t) (\_ t -> RawPassword t)
instance HasText Username where _text = lens (\(Username t) -> t) (\_ t -> Username t)
instance HasText Base64Content where _text = lens (\(Base64Content t) -> t) (\_ t -> Base64Content t)
instance HasText PublicKey where _text = base64Content . _text

class HasBase64Content a where base64Content :: Lens' a Base64Content
instance HasBase64Content Base64Content where base64Content = identity
instance HasBase64Content PublicKey where base64Content = lens (\(PublicKey b) -> b) (\_ c -> PublicKey c)

class HasRawPassword a where rawPassword :: Lens' a RawPassword
instance HasRawPassword RawPassword where rawPassword = identity
instance HasRawPassword LoginUser where rawPassword = lens (\(LoginUser _ p) -> p) (\(LoginUser u _) p -> LoginUser u p)
instance HasRawPassword RegisterUser where rawPassword = lens (\(RegisterUser _ p _) -> p) (\(RegisterUser u _ k) p -> RegisterUser u p k)

class HasUsername a where username :: Lens' a Username
instance HasUsername Username where username = identity
instance HasUsername LoginUser where username = lens (\(LoginUser u _) -> u) (\(LoginUser _ p) u -> LoginUser u p)
instance HasUsername RegisterUser where username = lens (\(RegisterUser u _ _) -> u) (\(RegisterUser _ p k) u -> RegisterUser u p k)

class HasPublicKey a where publicKey :: Lens' a PublicKey
