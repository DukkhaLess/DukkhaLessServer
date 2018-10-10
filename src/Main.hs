module Main where

import           Protolude (($), IO)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    html "Hi"