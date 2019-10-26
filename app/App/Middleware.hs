module App.Middleware where

import           Network.Wai.Middleware.Rewrite ( PathsAndQueries
                                                , rewritePureWithQueries
                                                )
import           Network.HTTP.Types.Header      ( RequestHeaders )
import           Network.Wai                    ( Middleware )

removeApiPrefix :: PathsAndQueries -> RequestHeaders -> PathsAndQueries
removeApiPrefix ("api" : tail, queries) _ = (tail, queries)
removeApiPrefix paq                     _ = paq

removePrefixMiddleware :: Middleware
removePrefixMiddleware = rewritePureWithQueries removeApiPrefix