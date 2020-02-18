{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>

module Endpoints
    ( startEndpoints,
    test1
    ) where

-- affine tes imports?
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Servant

import MoviesImporter
import Model
import RedisConnector

import Database.Redis

--type MoviesAPI = "import" :> Get '[JSON] [Movie]
type MoviesAPI = "import" :> Get '[PlainText] String

proxy :: Proxy MoviesAPI
proxy = Proxy


--server :: Server MoviesAPI
--server = do
--  movies <- liftIO $ importMovies
--  return movies

server :: Server MoviesAPI
server = do
  movies <- liftIO $ importMovies
--  answers <- liftIO $ saveMovies movies
  return "Import in progress"


app :: Application
app = serve proxy server


-- Single Movie fetching endpoint
type SingleMovieAPI = "movie" :> Capture "id" String :> Get '[JSON] Movie

singleMovieApi :: Proxy SingleMovieAPI
singleMovieApi = Proxy


-- Multiple Movies fetching endpoint
type MultipleMoviesAPI = "movies" :> QueryParams "id" [String] :> Get '[JSON] [Movie] --or return a Stream


test1 :: IO ()
test1 = do
  saveMovies movies1


startEndpoints :: IO ()
startEndpoints = run 8080 app


--                 .route(GET(URL_PREFIX + "/movie/{movieId}"), handler::getMovie)
   --                .andRoute(GET(URL_PREFIX + "/movies"), handler::getMovies)
   --                .andRoute(GET(URL_PREFIX + "/cache/refresh"), handler::refreshCache)
   --                .andRoute(GET("/readiness"), handler::isCacheReady);

