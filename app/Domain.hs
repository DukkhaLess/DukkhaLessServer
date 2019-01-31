module Domain where

import           Protolude
import           Control.Lens
import           Types
import           Crypto                         ( hashPassword )
import           Schema
import           Data.UUID                      ( UUID )
import           Data.Time.LocalTime            ( utcToLocalTime
                                                , utc
                                                )
import           Data.Time.Clock                ( getCurrentTime
                                                , UTCTime(..)
                                                )
import           Data.Time.Calendar             ( addDays )
import           System.Random                  ( randomIO )
import           Crypto.Argon2                  ( Argon2Status )

newUser :: RegisterUser -> PasswordSalt -> ExceptT Argon2Status IO User
newUser (RegisterUser name rawPass pubKey) (PasswordSalt salt) = do
  uuid       <- lift (randomIO :: IO UUID)
  hashedPass <- ExceptT $ pure $ hashPassword salt rawPass
  now        <- lift $ utcToLocalTime utc <$> getCurrentTime
  pure $ User uuid
              (name ^. usernameText)
              (hashedPass ^. hashedPasswordText)
              (pubKey ^. publicKeyBase64Content . base64ContentText)
              now
              now

createAccessToken :: User -> IO AccessToken
createAccessToken (User uuid _ _ _ _ _) = do
  inTwoDays <- getCurrentTime
    <&> \(UTCTime day dayTime) -> UTCTime (addDays 2 day) dayTime
  token <- randomIO <&> TokenId
  let uid = UserId uuid
  pure $ AccessToken token uid (Expiry inTwoDays)