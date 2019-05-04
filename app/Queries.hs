module Queries where

import Prelude
import           Control.Lens
import Data.ByteString (ByteString)
import Data.String.QQ
import Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Types
import Schema.Types
import Queries.Encoders
import Queries.Decoders

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
                    "userUsername"=$1 
            |]
        encoder = HE.param usernameValue

        userDecoder :: HD.Row User
        userDecoder =
            User
            <$> HD.column HD.uuid
            <*> HD.column HD.text
            <*> HD.column HD.text
            <*> HD.column HD.text

        decoder :: HD.Result (Maybe (Timestamped User))
        decoder = HD.rowMaybe $
            Timestamped <$> HD.column HD.timestamptz <*> HD.column HD.timestamptz <*> userDecoder

   
usernameValue :: HE.Value Username
usernameValue = contramap (^. usernameText) HE.text

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
            contramap (view (createT . userUuid)) (HE.param HE.uuid) <>
            contramap (view (createT . userUsername)) (HE.param HE.text) <>
            contramap (view (createT . userHashedPassword)) (HE.param HE.text) <>
            contramap (view (createT . userPublicKey)) (HE.param HE.text) <>
            contramap (view createLastUpdated) (HE.param HE.timestamptz) <>
            contramap (view createCreatedAt) (HE.param HE.timestamptz)
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
            contramap (view (createT . journalUuid)) (HE.param HE.uuid) <>
            contramap (view (createT . journalUserUuid)) (HE.param HE.uuid) <>
            contramap (view (createT . journalTitleContent)) (HE.param HE.text) <>
            contramap (view (createT . journalContent)) (HE.param HE.text) <>
            contramap (view (createLastUpdated)) (HE.param HE.timestamptz) <>
            contramap (view (createCreatedAt)) (HE.param HE.timestamptz)
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
            contramap (view (updateT . journalUuid)) (HE.param HE.uuid) <>
            contramap (view (updateT . journalUserUuid)) (HE.param HE.uuid) <>
            contramap (view (updateT . journalTitleContent)) (HE.param HE.text) <>
            contramap (view (updateT . journalContent)) (HE.param HE.text) <>
            contramap (view updateLastUpdated) (HE.param HE.timestamptz)
        decoder = HD.unit
