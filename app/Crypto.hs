module Crypto where

import           Prelude                        ( ($)
                                                , Either
                                                , fmap
                                                )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                )
import           Jose.Jwa                       ( JwsAlg(HS512) )
import           Jose.Jwt                       ( JwtError
                                                , Jwt
                                                )
import           Jose.Jws                       ( hmacEncode )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Crypto.Argon2                  ( verifyEncoded
                                                , hashEncoded
                                                , defaultHashOptions
                                                , Argon2Status
                                                )
import           Types                          ( RawPassword(..)
                                                , HashedPassword(..)
                                                , _text
                                                , SigningKey(..)
                                                )
import           Control.Lens

hashPassword :: ByteString -> RawPassword -> Either Argon2Status HashedPassword
hashPassword salt pass = fmap HashedPassword
  $ hashEncoded defaultHashOptions (encodeUtf8 (pass ^. _text)) salt

verifyPassword :: HashedPassword -> RawPassword -> Argon2Status
verifyPassword (HashedPassword pass) (RawPassword raw) =
  verifyEncoded pass (encodeUtf8 raw)

signJwt :: ToJSON a => SigningKey -> a -> Either JwtError Jwt
signJwt (SigningKey k) a = hmacEncode HS512 k (toStrict $ encode a)
