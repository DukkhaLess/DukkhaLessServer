module Queries where

import Prelude
import Control.Monad.Trans.Except
import Schema
import Types
import Hasql.Connection
import Hasql.Session

findUserByUsername :: Username -> Session User
findUserByUsername = _

insertUser :: User -> Session ()
insertUser = _