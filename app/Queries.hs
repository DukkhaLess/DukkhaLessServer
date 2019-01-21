module Queries where

import Prelude
import Control.Monad.Trans.Except
import Schema
import Types
import Hasql.Connection
import Hasql.Session
import Data.ByteString (ByteString)
import Data.String.QQ
import Hasql.Statement

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
        encoder = _
        decoder = _

insertUser :: User -> Session ()
insertUser = _