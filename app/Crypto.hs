module Crypto where

import           Protolude                      ( ($)
                                                , (<$>)
                                                , Either(..)
                                                , fmap
                                                , (.)
                                                , MonadIO
                                                , Int
                                                , pure
                                                , const
                                                , IO
                                                , (^)
                                                , either
                                                , Word64
                                                )
import           Control.Concurrent.STM         ( atomically
                                                , readTVarIO
                                                , modifyTVar'
                                                , newTVarIO
                                                )
import           Crypto.Classes.Exceptions      ( genBytes )
import qualified Control.Exception             as E
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT 
                                                )
import           Control.Monad.Reader
import           Control.Lens
import           Crypto.Random                  ( GenError )
import           Crypto.Random.DRBG             ( newGenAutoReseed )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                )
import           Data.ByteString                ( ByteString )
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
import           System.Entropy                 ( getEntropy )
import qualified Types                          as T
import qualified API.Types                      as API 

hashPassword :: T.PasswordSalt -> T.RawPassword -> Either Argon2Status T.HashedPassword
hashPassword (T.PasswordSalt salt) (T.RawPassword pass) =
  fmap T.HashedPassword
    $   toText
    <$> hashEncoded defaultHashOptions (encodeUtf8 pass) salt

verifyPassword :: T.HashedPassword -> T.RawPassword -> Argon2Status
verifyPassword (T.HashedPassword pass) (T.RawPassword raw) =
  verifyEncoded (fromText pass) (encodeUtf8 raw)

signJwt :: ToJSON a => T.SigningKey -> a -> Either JwtError API.SessionToken
signJwt (T.SigningKey k) a =
  (API.SessionToken . T.Base64Content . decodeUtf8 . unJwt)
    <$> hmacEncode HS512 k (toStrict $ encode a)

nextBytes
  :: MonadReader r m
  => MonadIO m
  => T.HasCryptoStore r T.CryptoStore
  => Int
  -> m ByteString
nextBytes byteCount = do
  tVar       <- asks (^. T.cryptoStore)
  currentGen <- liftIO $ readTVarIO tVar
  let (salt, nextGen) = genBytes byteCount currentGen
  liftIO $ atomically $ modifyTVar' tVar (const nextGen)
  pure salt

createGen ::  ExceptT GenError IO T.CryptoGen
createGen = do
  initialEntropy <- lift $ getEntropy 256
  let gen = newGenAutoReseed initialEntropy ((2 :: Word64) ^ (48 :: Word64))
  ExceptT $ return gen

createStoreOrFail :: IO T.CryptoStore
createStoreOrFail = do
  eitherGen <- runExceptT createGen
  gen   <- either E.throwIO return eitherGen
  newTVarIO gen
