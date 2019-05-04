module Domain where

import           Protolude
import           Types
import           Domain.Types
import           Crypto                         ( hashPassword )
import           Data.UUID                      ( UUID )
import           Data.Time.Clock                ( getCurrentTime
                                                , UTCTime(..)
                                                )
import           Data.Time.Calendar             ( addDays )
import           System.Random                  ( randomIO )
import           Crypto.Argon2                  ( Argon2Status )
import qualified Schema.Types as Schema

newUser :: RegisterUser -> PasswordSalt -> ExceptT Argon2Status IO (Schema.Created User)
newUser (RegisterUser name rawPass pubKey) (PasswordSalt salt) = do
  uuid       <- lift (randomIO :: IO UUID)
  hashedPass <- ExceptT $ pure $ hashPassword salt rawPass
  now <- lift $ getCurrentTime
  create $ User
    (UserId uuid)
    name
    hashedPass
    pubKey

createAccessToken :: UserId -> IO AccessToken
createAccessToken (UserId uuid) = do
  inTwoDays <- getCurrentTime
    <&> \(UTCTime day dayTime) -> UTCTime (addDays 2 day) dayTime
  token <- randomIO <&> TokenId
  let uid = UserId uuid
  pure $ AccessToken token uid (Expiry inTwoDays)