module Queries where

import Prelude
import Control.Monad.Trans.Except
import           Control.Lens
import Schema
import Types
import Hasql.Connection
import Hasql.Session
import Data.ByteString (ByteString)
import Data.String.QQ
import Hasql.Statement
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD

findUserByUsername :: Statement Username (Maybe User)
findUserByUsername = Statement sqlS encoder decoder True
    where
        sqlS :: ByteString
        sqlS =  
            [s|SELECT
                    userUuid,
                    userUsername,
                    userHashedPassword,
                    userPublicKey,
                    userLastUpdated,
                    userCreatedAt
                FROM
                    users
                WHERE
                    userUsername=$1 |]
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

insertUser :: User -> Session ()
insertUser = _

usernameValue :: HE.Value Username
usernameValue = contramap (^. _text) HE.text