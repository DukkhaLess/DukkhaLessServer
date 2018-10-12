module Types where

import Protolude

newtype Username = Username Text
newtype Password = Password Text
newtype Base64Content = Base64Content Text
newtype PublicKey = PublicKey Base64Content

data User
  = User
    { username :: Username
    , password :: Password
    , publicKey :: PublicKey
    }

newtype SessionToken = SessionToken Base64Content