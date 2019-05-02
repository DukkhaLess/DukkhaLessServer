module Domain where

import           Protolude
import           Control.Lens
import           Types
import           Domain.Types
import           Crypto                         ( hashPassword )
import           Schema.Types
import           Data.UUID                      ( UUID )
import           Data.Time.Clock                ( getCurrentTime
                                                , UTCTime(..)
                                                )
import           Data.Time.Calendar             ( addDays )
import           System.Random                  ( randomIO )
import           Crypto.Argon2                  ( Argon2Status )

newUser :: RegisterUser -> PasswordSalt -> ExceptT Argon2Status IO (Create User)
newUser (RegisterUser name rawPass pubKey) (PasswordSalt salt) = do
  uuid       <- lift (randomIO :: IO UUID)
  hashedPass <- ExceptT $ pure $ hashPassword salt rawPass
  lift $ createIO $ User
    uuid
    (name ^. usernameText)
    (hashedPass ^. hashedPasswordText)
    (pubKey ^. publicKeyBase64Content . base64ContentText)

createAccessToken :: User -> IO AccessToken
createAccessToken (User uuid _ _ _) = do
  inTwoDays <- getCurrentTime
    <&> \(UTCTime day dayTime) -> UTCTime (addDays 2 day) dayTime
  token <- randomIO <&> TokenId
  let uid = UserId uuid
  pure $ AccessToken token uid (Expiry inTwoDays)


updateIO :: forall a . a -> IO (Update a)
updateIO a = Update <<< getCurrentTime <$> a

createIO :: forall a . a -> IO (Create a)
createIO a = do
  now <- getCurrentTime
  pure $ Create now now a


updatedJournalEntry :: UserId -> UpdateJournalEntry -> IO (Update Journal)
updatedJournalEntry owner entry = do
  let uuid = entry ^. updateJournalEntryJournalId . journalIdUUID
  updateIO $ Journal
    uuid
    (owner ^. userIdUUID)
    (  entry
    ^. updateJournalEntryTitleCiphertext
    .  titleCiphertext
    .  titleCiphertextEncryptedMessage
    . encryptedMessageText
    )
    (  entry
    ^. updateJournalEntryBodyCiphertext
    .  bodyCiphertext
    .  bodyCiphertextEncryptedMessage
    . encryptedMessageText
    )


createJournalEntry :: UserId -> CreateJournalEntry -> IO (Create Journal)
createJournalEntry owner entry = do
  uuid <- randomIO :: IO UUID
  createIO $ Journal
    uuid
    (owner ^. userIdUUID)
    (  entry
    ^. createJournalEntryTitleCiphertext 
    .  titleCiphertext
    .  titleCiphertextEncryptedMessage
    . encryptedMessageText
    )
    (  entry
    ^. createJournalEntryBodyCiphertext
    .  bodyCiphertext
    .  bodyCiphertextEncryptedMessage
    . encryptedMessageText
    )
