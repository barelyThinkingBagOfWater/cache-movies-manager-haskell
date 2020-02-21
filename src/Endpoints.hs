{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>

module Endpoints
    ( startEndpoints
    ) where

-- affine et clean tes imports?
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Servant

import MoviesImporter
import Model
import RedisConnector

import qualified Data.ByteString.Char8      as  C


--type MoviesAPI = "import" :> Get '[JSON] [Movie]
type MoviesAPI = "import" :> Get '[PlainText] String
               :<|> "movie" :> Get '[PlainText] String

importProxy :: Proxy MoviesAPI
importProxy = Proxy

importServer :: Server MoviesAPI
importServer = importServer
           :<|> singleMovieServer
  where  importServer = do
                          movies <- liftIO $ importMovies
                          liftIO $ saveMovies movies
                          return "Import in progress\n"

         singleMovieServer = do
                          encodedMovie <- liftIO $ getMovie 51094
                          liftIO $ print encodedMovie
                          return "lol\n"




app :: Application
app = serve importProxy importServer




-- Multiple Movies fetching endpoint
type MultipleMoviesAPI = "movies" :> QueryParams "id" [String] :> Get '[JSON] [Movie] --or return a Stream


startEndpoints :: IO ()
startEndpoints = run 8080 app

-- for metrics : https://hackage.haskell.org/package/prometheus-metrics-ghc

--                 .route(GET(URL_PREFIX + "/movie/{movieId}"), handler::getMovie)
   --                .andRoute(GET(URL_PREFIX + "/movies"), handler::getMovies)
   --                .andRoute(GET(URL_PREFIX + "/cache/refresh"), handler::refreshCache)
   --                .andRoute(GET("/readiness"), handler::isCacheReady);
