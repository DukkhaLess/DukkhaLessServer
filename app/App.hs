{-# LANGUAGE OverloadedStrings #-}
module App where

import           Protolude                      ( IO
                                                , ($)
                                                , (.)
                                                )
import           Control.Lens
import           Web.Scotty
import           Types

app :: IO ()
app = scotty 3000 $ do
  get "/:word" $ html "Hi"
  post "/login" $ do
    loginUser <- jsonData :: ActionM LoginUser
    text $ loginUser ^. (username . _text)
  post "/register" $ do
    registerUser <- jsonData :: ActionM RegisterUser
    text $ registerUser ^. (username . _text)
