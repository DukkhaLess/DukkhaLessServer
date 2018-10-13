module Main where

import Protolude          (IO, head, fromMaybe, (>>=))
import Text.Read          (readMaybe)
import System.Environment (getArgs)
import App                (app)
import Conf               (Environment(..))

main :: IO ()
main = do
  args <- getArgs
  let envArg = head args
  let maybeEnv = envArg >>= readMaybe
  let env = fromMaybe Production maybeEnv
  app env