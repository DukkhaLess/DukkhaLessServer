module Domain where

import           Protolude
import           Schema
import           Control.Lens
import           Types
import Crypto(hashPassword)
import Data.UUID(UUID)
import Data.Time.LocalTime(utcToLocalTime, utc)
import Data.Time.Clock(getCurrentTime)
import System.Random (randomIO)
import           Crypto.Argon2                  ( Argon2Status )

newUser :: RegisterUser -> PasswordSalt -> ExceptT Argon2Status IO User
newUser (RegisterUser name rawPass pubKey) (PasswordSalt salt) = do
  uuid <- lift (randomIO :: IO UUID)
  hashedPass <- ExceptT $ pure $ hashPassword salt rawPass
  now  <- lift $ utcToLocalTime utc <$> getCurrentTime
  pure $ User uuid (name ^. _text) (hashedPass ^. _text) (pubKey ^. _text) now
