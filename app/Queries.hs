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

        encoder = HE.param QE.username

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
            contramap (view (createT . userUserId)) (HE.param QE.userId) <>
            contramap (view (createT . userUsername)) (HE.param QE.username) <>
            contramap (view (createT . userHashedPassword)) (HE.param QE.hashedPassword) <>
            contramap (view (createT . userPublicKey)) (HE.param QE.publicKey) <>
            contramap (view createLastUpdated) (HE.param QE.lastUpdated) <>
            contramap (view createCreatedAt) (HE.param QE.createdAt)
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
            contramap (view (createT . journalJournalId)) (HE.param QE.journalId) <>
            contramap (view (createT . journalUserId)) (HE.param QE.userId) <>
            contramap (view (createT . journalTitleContent)) (HE.param QE.titleCiphertext) <>
            contramap (view (createT . journalContent)) (HE.param QE.bodyCiphertext) <>
            contramap (view (createLastUpdated)) (HE.param QE.lastUpdated) <>
            contramap (view (createCreatedAt)) (HE.param QE.createdAt)
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
            contramap (view (updateT . journalJournalId)) (HE.param QE.journalId) <>
            contramap (view (updateT . journalUserId)) (HE.param QE.userId) <>
            contramap (view (updateT . journalTitleContent)) (HE.param QE.titleCiphertext) <>
            contramap (view (updateT . journalContent)) (HE.param QE.bodyCiphertext) <>
            contramap (view updateLastUpdated) (HE.param QE.lastUpdated)
