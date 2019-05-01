module Queries where

import Prelude
import           Control.Lens
import Control.Arrow
import Schema
import Data.ByteString (ByteString)
import Data.String.QQ
import Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import           Data.Text                      ( Text )
import Types
import Schema.Types

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

usernameValue :: HE.Value Username
usernameValue = contramap (^. _usernameText) HE.text

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
            contramap (view _userUuid) (HE.param HE.uuid) <>
            contramap (view _userUsername) (HE.param HE.text) <>
            contramap (view _userHashedPassword) (HE.param HE.text) <>
            contramap (view _userPublicKey) (HE.param HE.text) <>
            contramap (view _userLastUpdated) (HE.param HE.timestamp) <>
            contramap (view _userCreatedAt) (HE.param HE.timestamp)
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
            contramap (view (_createT . _journalUuid)) (HE.param HE.uuid) <>
            contramap (view (_createT . _journalUserUuid)) (HE.param HE.uuid) <>
            contramap (view (_createT . _journalTitleContent)) (HE.param HE.text) <>
            contramap (view (_createT . _journalContent)) (HE.param HE.text) <>
            contramap (view (_createLastUpdated)) (HE.param HE.timestamp) <>
            contramap (view (_createCreatedAt)) (HE.param HE.timestamp)
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
            contramap (view (_updateT . _journalUuid)) (HE.param HE.uuid) <>
            contramap (view (_updateT . _journalUserUuid)) (HE.param HE.uuid) <>
            contramap (view (_updateT . _journalTitleContent)) (HE.param HE.text) <>
            contramap (view (_updateT . _journalContent)) (HE.param HE.text) <>
            contramap (view _updateLastUpdated) (HE.param HE.timestamp)
        decoder = HD.unit
