{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>

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


startApp :: IO ()
startApp = run 8080 app


-- Test
type TestMovieAPI = "test" :> Get '[JSON] [Movie]

testApi :: Proxy TestMovieAPI
testApi = Proxy

testServer :: Server TestMovieAPI
testServer = return movies1

app :: Application
app = serve testApi testServer

-- Single Movie fetching endpoint
type SingleMovieAPI = "movie" :> Capture "id" String :> Get '[JSON] Movie

singleMovieAPI :: Proxy SingleMovieAPI
singleMovieAPI = Proxy

--singleMovieServer :: Server SingleMovieAPI
--singleMovieServer = return singleMovie

-- Multiple Movies fetching endpoint
type MultipleMoviesAPI = "movies" :> QueryParams "id" [String] :> Get '[JSON] [Movie] --or return a Stream


singleMovie :: Movie
singleMovie = Movie 1 "title1" "genres1" ["tag11", "tag12"]

movies1 :: [Movie]
movies1 = [ Movie 1 "title1" "genres1" ["tag11", "tag12"]
         , Movie 2 "title2" "genres2" ["tag21", "tag22"]
         ]

--                 .route(GET(URL_PREFIX + "/movie/{movieId}"), handler::getMovie)
   --                .andRoute(GET(URL_PREFIX + "/movies"), handler::getMovies)
   --                .andRoute(GET(URL_PREFIX + "/cache/refresh"), handler::refreshCache)
   --                .andRoute(GET("/readiness"), handler::isCacheReady);