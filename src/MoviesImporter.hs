{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TypeOperators   #-} -- for the :>

module MoviesImporter (importMovies) where

import Data.Aeson
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Servant
import Model

import qualified Servant.Client.Streaming as S


type MoviesManagerAPI = "movies-manager" :> "movies" :> Get '[JSON] [Movie]

moviesManagerAPI :: Proxy MoviesManagerAPI
moviesManagerAPI = Proxy

importMoviesClient :: ClientM [Movie]
importMoviesClient = client moviesManagerAPI


importMovies :: IO [Movie]
importMovies = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM importMoviesClient (mkClientEnv manager' (BaseUrl Http "172.18.42.4" 80 ""))
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return []
    Right (movies) -> do -- better handling including errors please
      return movies
