{-# LANGUAGE OverloadedStrings #-}
module App where

import           Protolude                      ( IO
                                                , ($)
                                                , (.)
                                                , (>>=)
                                                , Maybe(..)
                                                , putStrLn
                                                , show
                                                , (++)
                                                )
import           Control.Lens
import           Data.Text.Lazy                 ( unpack )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdoutDev )
import           Network.Wai.Middleware.Rewrite ( PathsAndQueries
                                                , rewritePureWithQueries
                                                )
import           Network.HTTP.Types.Header      ( RequestHeaders )
import           Web.Scotty
import           Types
import qualified Conf                          as Conf
import qualified Data.Configurator             as C

app :: Conf.Environment -> IO ()
app env = do
  config <-
    C.load [C.Required $ unpack $ Conf.confFileName env] >>= Conf.makeConfig
  case config of
    Just conf -> app' conf
    Nothing -> putStrLn $ "Config file not found for environment: " ++ show env

app' :: Conf.Config -> IO ()
app' _ = scotty 4000 $ do
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
