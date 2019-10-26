module Main where

import           Protolude                      ( IO
                                                , head
                                                , fromMaybe
                                                , (>>=)
                                                , ($)
                                                , putStrLn
                                                )
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )
import           App                            ( app )
import           Conf                           ( Environment(..) )
import           Data.String                    ( String )

main :: IO ()
main = do
  putStrLn ("Dukkhaless Backend Server App!" :: String)
  args <- getArgs
  let env = fromMaybe Production $ head args >>= readMaybe
  app env
