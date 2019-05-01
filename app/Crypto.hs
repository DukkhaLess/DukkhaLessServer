module Crypto where

import           Protolude                      ( ($)
                                                , (<$>)
                                                , Either(..)
                                                , fmap
                                                , (.)
                                                )
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                )
import           Data.Text.Short                ( fromText
                                                , toText
                                                )
import           Jose.Jwa                       ( JwsAlg(HS512) )
import           Jose.Jwt                       ( JwtError
                                                , unJwt
                                                )
import           Jose.Jws                       ( hmacEncode )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Crypto.Argon2                  ( verifyEncoded
                                                , hashEncoded
                                                , defaultHashOptions
                                                , Argon2Status
                                                )
import           Types                          ( RawPassword(..)
                                                , HashedPassword(..)
                                                , Base64Content(..)
                                                )
import           Domain.Types                   ( SigningKey(..)
                                                , SessionToken(..)
                                                )

hashPassword :: ByteString -> RawPassword -> Either Argon2Status HashedPassword
hashPassword salt (RawPassword pass) =
  fmap HashedPassword
    $   toText
    <$> hashEncoded defaultHashOptions (encodeUtf8 pass) salt

verifyPassword :: HashedPassword -> RawPassword -> Argon2Status
verifyPassword (HashedPassword pass) (RawPassword raw) =
  verifyEncoded (fromText pass) (encodeUtf8 raw)

signJwt :: ToJSON a => SigningKey -> a -> Either JwtError SessionToken
signJwt (SigningKey k) a =
  (SessionToken . Base64Content . decodeUtf8 . unJwt)
    <$> hmacEncode HS512 k (toStrict $ encode a)
