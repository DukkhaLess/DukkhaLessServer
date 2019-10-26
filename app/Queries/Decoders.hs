
{-# LANGUAGE Rank2Types #-}
module Queries.Decoders where

import           Protolude
import qualified Hasql.Decoders                as HD
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import qualified Types                         as T

valuatedText :: forall a . (Text -> a) -> HD.Value a
valuatedText f = fmap f HD.text

hashedPassword :: HD.Value T.HashedPassword
hashedPassword = valuatedText T.HashedPassword

username :: HD.Value T.Username
username = valuatedText T.Username

base64Content :: HD.Value T.Base64Content
base64Content = valuatedText T.Base64Content

publicKey :: HD.Value T.PublicKey
publicKey = valuatedText (T.PublicKey . T.Base64Content)

valueTime :: forall a . (UTCTime -> a) -> HD.Value a
valueTime f = fmap f HD.timestamptz

createdAt :: HD.Value T.CreatedAt
createdAt = valueTime T.CreatedAt

lastUpdated :: HD.Value T.LastUpdated
lastUpdated = valueTime T.LastUpdated

titleCiphertext :: HD.Value T.TitleCiphertext
titleCiphertext =
  valuatedText (T.TitleCiphertext . T.EncryptedMessage)

bodyCiphertext :: HD.Value T.BodyCiphertext
bodyCiphertext =
  valuatedText (T.BodyCiphertext . T.EncryptedMessage)

valuatedUUID :: forall a . (UUID -> a) -> HD.Value a
valuatedUUID f = fmap f HD.uuid

userId :: HD.Value T.UserId
userId = valuatedUUID T.UserId

journalId :: HD.Value T.JournalId
journalId = valuatedUUID T.JournalId
