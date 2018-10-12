{-# LANGUAGE OverloadedStrings #-}
module App where

import           Protolude                      ( ($)
                                                , IO
                                                )
import           Web.Scotty

app :: IO ()
app = scotty 3000 $ do
  get "/:word" $ do
    html "Hi"
