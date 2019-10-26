module Queries where

import Protolude
import           Control.Lens
import Data.ByteString (ByteString)
import Data.String.QQ
import Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Queries.Decoders as QD
import qualified Queries.Encoders as QE
import qualified Types as T
import Schema.Types

findUserByUsername :: Statement T.Username (Maybe (Timestamped User))
findUserByUsername = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS =  
            [s|SELECT
                    "userUuid",
                    "userUsername",
                    "userHashedPassword",
                    "userPublicKey",
                    "userLastUpdated",
                    "userCreatedAt"
                FROM
                    "users"
                WHERE
                    "userUsername"=$1 
            |]

        encoder = HE.param QE.text

        decodeFields id username hp pk ca lu = Timestamped lu ca (User id username hp pk)

        decoder = HD.rowMaybe $
            decodeFields <$> HD.column QD.userId
            <*> HD.column QD.username
            <*> HD.column QD.hashedPassword
            <*> HD.column QD.publicKey
            <*> HD.column  QD.lastUpdated
            <*> HD.column QD.createdAt

insertUser :: Statement (Create User) ()
insertUser = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS =
            [s|
                INSERT INTO "users"
                (
                    "userUuid",
                    "userUsername",
                    "userHashedPassword",
                    "userPublicKey",
                    "userLastUpdated",
                    "userCreatedAt"
                )
                VALUES (
                  $1, $2, $3, $4, $5, $6
                )
            |]
        encoder =
            contramap (view (createT . userUserId)) (HE.param QE.uuid) <>
            contramap (view (createT . userUsername)) (HE.param QE.text) <>
            contramap (view (createT . userHashedPassword)) (HE.param QE.text) <>
            contramap (view (createT . userPublicKey . T.base64Content)) (HE.param QE.text) <>
            contramap (view createLastUpdated) (HE.param QE.timestamptz) <>
            contramap (view createCreatedAt) (HE.param QE.timestamptz)
        decoder = HD.unit

insertJournal :: Statement (Create Journal) ()
insertJournal = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS = 
            [s|
                INSERT INTO "journalEntries"
                (
                    "journalUuid",
                    "userUuid",
                    "journalTitleContent",
                    "journalContent", 
                    "journalLastUpdated",
                    "journalCreatedAt"
                )
                VALUES (
                    $1, $2, $3, $4, $5, $6
                )
            |]
        encoder =
            contramap (view (createT . journalJournalId)) (HE.param QE.uuid) <>
            contramap (view (createT . journalUserId)) (HE.param QE.uuid) <>
            contramap (view (createT . journalTitleContent . T.encryptedMessage)) (HE.param QE.text) <>
            contramap (view (createT . journalContent . T.encryptedMessage)) (HE.param QE.text) <>
            contramap (view (createLastUpdated)) (HE.param QE.timestamptz) <>
            contramap (view (createCreatedAt)) (HE.param QE.timestamptz)
        decoder = HD.unit

updateJournal :: Statement (Update Journal) ()
updateJournal = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS = 
            [s|
                UPDATE "journalEntries"
                SET (
                    "journalTitleContent",
                    "journalContent", 
                    "journalLastUpdated",
                ) = ($3, $4, $5)
                WHERE "journalUuid" = $1
                WHERE "journalUserUuid" = $2
            |]
        decoder = HD.unit
        encoder =
            contramap (view (updateT . journalJournalId)) (HE.param QE.uuid) <>
            contramap (view (updateT . journalUserId)) (HE.param QE.uuid) <>
            contramap (view (updateT . journalTitleContent . T.encryptedMessage)) (HE.param QE.text) <>
            contramap (view (updateT . journalContent . T.encryptedMessage)) (HE.param QE.text) <>
            contramap (view updateLastUpdated) (HE.param QE.timestamptz)
