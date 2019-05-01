module Queries where

import Prelude
import           Control.Lens
import Control.Arrow
import Schema
import Types
import Data.ByteString (ByteString)
import Data.String.QQ
import Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

findUserByUsername :: Statement Username (Maybe (Timestamped User))
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
                    "userUsername"=$1 |]
        encoder = HE.param usernameValue
        decoder = 
            HD.rowMaybe
            $ User
            <$> HD.column HD.uuid
            <*> HD.column HD.text
            <*> HD.column HD.text
            <*> HD.column HD.text
            <*> HD.column HD.timestamp
            <*> HD.column HD.timestamp

insertUser :: Statement User ()
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
            contramap _userUuid (HE.param HE.uuid) <>
            contramap _userUsername (HE.param HE.text) <>
            contramap _userHashedPassword (HE.param HE.text) <>
            contramap _userPublicKey (HE.param HE.text) <>
            contramap _userLastUpdated (HE.param HE.timestamp) <>
            contramap _userCreatedAt (HE.param HE.timestamp)
        decoder = HD.unit

usernameValue :: HE.Value Username
usernameValue = contramap (^. usernameText) HE.text

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
            contramap (_createT >>> _journalUuid) (HE.param HE.uuid) <>
            contramap (_createT >>> _journalUserUuid) (HE.param HE.uuid) <>
            contramap (_createT >>> _journalTitleContent) (HE.param HE.text) <>
            contramap (_createT >>> _journalContent) (HE.param HE.text) <>
            contramap (_createLastUpdated) (HE.param HE.timestamp) <>
            contramap (_createCreatedAt) (HE.param HE.timestamp)
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
        encoder =
            contramap (_updateT >>> _journalUuid) (HE.param HE.uuid) <>
            contramap (_updateT >>> _journalUserUuid) (HE.param HE.uuid) <>
            contramap (_updateT >>> _journalTitleContent) (HE.param HE.text) <>
            contramap (_updateT >>> _journalContent) (HE.param HE.text) <>
            contramap _updateLastUpdated (HE.param HE.timestamp)
        decoder = HD.unit
