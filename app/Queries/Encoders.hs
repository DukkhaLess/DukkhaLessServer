{-# LANGUAGE Rank2Types #-}
module Queries.Encoders where

import           Protolude
import           Data.Functor.Contravariant     ( contramap )
import           Control.Lens
import qualified Hasql.Encoders                as HE
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import qualified Types                         as T

valuatedText :: forall a . Lens' a Text -> HE.Value a
valuatedText l = contramap (^. l) HE.text

hashedPassword :: HE.Value T.HashedPassword
hashedPassword = valuatedText T.hashedPasswordText

username :: HE.Value T.Username
username = valuatedText T.usernameText

base64Content :: HE.Value T.Base64Content
base64Content = valuatedText T.base64ContentText

publicKey :: HE.Value T.PublicKey
publicKey = valuatedText (T.publicKeyBase64Content . T.base64ContentText)

valueTime :: forall a. Lens' a UTCTime -> HE.Value a
valueTime l = contramap (^. l) HE.timestamptz

createdAt :: HE.Value T.CreatedAt
createdAt = valueTime T.createdAtUTCTime

lastUpdated :: HE.Value T.LastUpdated
lastUpdated = valueTime T.lastUpdatedUTCTime

titleCiphertext :: HE.Value T.TitleCiphertext
titleCiphertext = valuatedText (T.titleCiphertextEncryptedMessage . T.encryptedMessageText)

bodyCiphertext :: HE.Value T.BodyCiphertext
bodyCiphertext = valuatedText (T.bodyCiphertextEncryptedMessage . T.encryptedMessageText)

valuatedUUID :: forall a. Lens' a UUID -> HE.Value a
valuatedUUID l = contramap (^. l) HE.uuid

userId :: HE.Value T.UserId
userId = valuatedUUID T.userIdUUID

journalId :: HE.Value T.JournalId
journalId = valuatedUUID T.journalIdUUID