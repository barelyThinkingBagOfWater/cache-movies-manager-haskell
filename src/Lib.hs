{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import MoviesImporter
import Model





-- https://haskell-servant.readthedocs.io/en/v0.16/tutorial/Server.html

-- Test
type TestMovieAPI = "test" :> Get '[JSON] [Movie]

testApi :: Proxy TestMovieAPI
testApi = Proxy

testServer :: Server TestMovieAPI
testServer = return movies1

--app :: Application
--app = serve testApi testServer


-- Position
type API = "test" :> Get '[JSON] [Movie]
         :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  }
$(deriveJSON defaultOptions ''Position)

server3 :: Server API
server3 = return movies1
     :<|> position
  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server3



-- Single Movie fetching endpoint
type SingleMovieAPI = "movie" :> Capture "id" String :> Get '[JSON] Movie

singleMovieApi :: Proxy SingleMovieAPI
singleMovieApi = Proxy


--singleMovieServer :: Server SingleMovieAPI
--singleMovieServer = return singleMovie
--  where singleMovie :: String -> Movie
--        singleMovie id = return (Movie 1 "title5" "genres1" ["tag11", "tag12"])


--app :: Application
--app = serve singleMovieApi singleMovieServer

-- Multiple Movies fetching endpoint
type MultipleMoviesAPI = "movies" :> QueryParams "id" [String] :> Get '[JSON] [Movie] --or return a Stream


startApp :: IO ()
startApp = run 8080 app


--                 .route(GET(URL_PREFIX + "/movie/{movieId}"), handler::getMovie)
   --                .andRoute(GET(URL_PREFIX + "/movies"), handler::getMovies)
   --                .andRoute(GET(URL_PREFIX + "/cache/refresh"), handler::refreshCache)
   --                .andRoute(GET("/readiness"), handler::isCacheReady);

