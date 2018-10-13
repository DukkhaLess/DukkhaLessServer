{-# LANGUAGE OverloadedStrings #-}
module App where

import           Protolude                      ( IO
                                                , ($)
                                                , (.)
                                                )
import           Control.Lens
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Network.Wai.Middleware.Rewrite ( PathsAndQueries
                                                , rewritePureWithQueries
                                                )
import           Network.HTTP.Types.Header      ( RequestHeaders )
import           Web.Scotty
import           Types

app :: IO ()
app = scotty 4000 $ do
  middleware $ rewritePureWithQueries removeApiPrefix
  middleware logStdoutDev
  get "/:word" $ html "Hi"
  post "/login" $ do
    loginUser <- jsonData :: ActionM LoginUser
    text $ loginUser ^. (username . _text)
  post "/register" $ do
    registerUser <- jsonData :: ActionM RegisterUser
    text $ registerUser ^. (username . _text)

removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix (("api" : tail), queries) _ = (tail, queries)
removeApiPrefix paq                       _ = paq
