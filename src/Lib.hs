{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Movie)

--newtype MovieIds = MovieIds { ids :: [Int] }

--newtype MovieId = MovieId { id :: String }

type API1 = "movies" :> Get '[JSON] [Movie]

type API2 = "movies2" :> Get '[JSON] [Movie]
  :<|> "moviestest" :> QueryParam "id" String :> Get '[JSON] [Movie]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server1

api :: Proxy API1
api = Proxy

server1 :: Server API1
server1 = return movies1

server2 :: Server API2
server2 = movies2
    :<|> moviestest

    where movies2 = return movies1

          moviestest :: Maybe String -> Handler [Movie]
          moviestest id = return . [Movie] $ case id of
            Nothing -> movies1
            Just n -> movies1

movies1 :: [Movie]
movies1 = [ Movie 1 "title1" "genres1" ["tag11", "tag12"]
         , Movie 2 "title2" "genres2" ["tag21", "tag22"]
         ]

--                 .route(GET(URL_PREFIX + "/movie/{movieId}"), handler::getMovie)
   --                .andRoute(GET(URL_PREFIX + "/movies"), handler::getMovies)
   --                .andRoute(GET(URL_PREFIX + "/cache/refresh"), handler::refreshCache)
   --                .andRoute(GET("/readiness"), handler::isCacheReady);