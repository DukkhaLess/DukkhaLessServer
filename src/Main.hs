module Main where

import           Protolude                      ( IO
                                                , head
                                                , fromMaybe
                                                , (>>=)
                                                )
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )
import           App                            ( app )
import           Conf                           ( Environment(..) )

main :: IO ()
main = do
  args <- getArgs
  let env = fromMaybe Production $ head args >>= readMaybe
  app env
