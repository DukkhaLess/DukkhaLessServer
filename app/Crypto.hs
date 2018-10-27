module Crypto where

import Prelude (($), Either)
import           Data.Text.Short                ( ShortText
                                                , toText
                                                )
import           Data.Text                      ( Text )
import Data.Text.Encoding (encodeUtf8)
import           Crypto.Argon2                  ( verifyEncoded
                                                , hashEncoded
                                                , defaultHashOptions
                                                , Argon2Status
                                                )
import           Types                          ( RawPassword
                                                , HashedPassword
                                                , _text
                                                )
import           Control.Lens

hashPassword :: RawPassword -> Either Argon2Status HashedPassword
hashPassword pass = hashEncoded defaultHashOptions (encodeUtf8 (pass ^. _text))
