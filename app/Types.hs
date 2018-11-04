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
                                                , (<>)
                                                )
import           Control.Lens.Combinators
import           Data.ByteString                ( ByteString )
import           Data.Aeson
import           Data.Aeson.Types               ( typeMismatch )
import           Data.Text                      ( Text )
import           Data.Text.Short                ( ShortText
                                                , fromText
                                                , toText
                                                )
import           Data.UUID                      ( UUID )
import           Data.Time.Clock                ( UTCTime )

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

newtype HashedPassword = HashedPassword ShortText

class HasText a where _text :: Lens' a Text
instance HasText Text where _text = identity
instance HasText RawPassword where _text = lens (\(RawPassword t) -> t) (\_ t -> RawPassword t)
instance HasText Username where _text = lens (\(Username t) -> t) (\_ t -> Username t)
instance HasText Base64Content where _text = lens (\(Base64Content t) -> t) (\_ t -> Base64Content t)
instance HasText PublicKey where _text = base64Content . _text
instance HasText HashedPassword where _text = lens (\(HashedPassword st) -> toText st) (\_ t -> HashedPassword (fromText t))

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
instance HasPublicKey RegisterUser where publicKey = lens (\(RegisterUser _ _ k) -> k) (\(RegisterUser u p _) k -> RegisterUser u p k)

newtype PasswordSalt = PasswordSalt ByteString

newtype TokenId = TokenId UUID
    deriving (Eq, Show, Generic)
instance ToJSON TokenId
instance FromJSON TokenId

class HasTokenId a where tokenId :: Lens' a TokenId
instance HasTokenId TokenId where tokenId = identity
instance HasTokenId AccessToken where tokenId = lens (\(AccessToken ti _ _) -> ti) (\(AccessToken _ ui t) ti -> AccessToken ti ui t)

newtype UserId = UserId UUID
    deriving (Eq, Show, Generic)
instance ToJSON UserId
instance FromJSON UserId

class HasUserId a where userId :: Lens' a UserId
instance HasUserId UserId where userId = identity
instance HasUserId AccessToken where userId = lens (\(AccessToken _ ui _) -> ui) (\(AccessToken ti _ t) ui -> AccessToken ti ui t)

class HasUUID a where _uuid :: Lens' a UUID
instance HasUUID UUID where _uuid = identity
instance HasUUID TokenId where _uuid = lens (\(TokenId i) -> i) (\_ i -> TokenId i)
instance HasUUID UserId where _uuid = lens (\(UserId i) -> i) (\_ i -> UserId i)

newtype Expiry = Expiry UTCTime
    deriving (Eq, Show, Generic)
instance ToJSON Expiry
instance FromJSON Expiry

class HasExpiry a where expiry :: Lens' a Expiry
instance HasExpiry Expiry where expiry = identity
instance HasExpiry AccessToken where expiry = lens (\(AccessToken _ _ t) -> t) (\(AccessToken ti ui _) t -> AccessToken ti ui t)

class HasUtcTime a where utcTime :: Lens' a UTCTime
instance HasUtcTime Expiry where utcTime = lens (\(Expiry t) -> t) (\_ t -> Expiry t)

data AccessToken =
  AccessToken TokenId UserId Expiry
    deriving (Eq, Show, Generic)
instance ToJSON AccessToken where
  toJSON (AccessToken ti ui e) = object [ "jti" .= ti, "sub" .= ui, "exp" .= e]
  toEncoding (AccessToken ti ui e) = pairs ("jti" .= ti <> "sub" .= ui <> "exp" .= e)
instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken
    <$> v .: "jti"
    <*> v .: "sub"
    <*> v .: "exp"
  parseJSON invalid = typeMismatch "AccessToken" invalid
