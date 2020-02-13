{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>
{-# LANGUAGE OverloadedStrings #-}

module Endpoints
    ( startEndpoints
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import MoviesImporter
import Model
import Control.Monad.IO.Class


type MoviesAPI = "import" :> Get '[JSON] [Movie]

proxy :: Proxy MoviesAPI
proxy = Proxy


server :: Server MoviesAPI
--server = return movies1
server = do
  movies <- liftIO $ importMovies
  return movies



app :: Application
app = serve proxy server



-- Single Movie fetching endpoint
type SingleMovieAPI = "movie" :> Capture "id" String :> Get '[JSON] Movie

singleMovieApi :: Proxy SingleMovieAPI
singleMovieApi = Proxy


-- Multiple Movies fetching endpoint
type MultipleMoviesAPI = "movies" :> QueryParams "id" [String] :> Get '[JSON] [Movie] --or return a Stream


startEndpoints :: IO ()
startEndpoints = run 8080 app


--                 .route(GET(URL_PREFIX + "/movie/{movieId}"), handler::getMovie)
   --                .andRoute(GET(URL_PREFIX + "/movies"), handler::getMovies)
   --                .andRoute(GET(URL_PREFIX + "/cache/refresh"), handler::refreshCache)
   --                .andRoute(GET("/readiness"), handler::isCacheReady);
