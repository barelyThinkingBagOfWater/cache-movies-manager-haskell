{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>

module MoviesImporter (run2) where

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

importMovies :: ClientM [Movie]
importMovies = client moviesManagerAPI


run2 :: IO ()
run2 = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM importMovies (mkClientEnv manager' (BaseUrl Http "172.18.42.4" 80 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (movies) -> do
      print movies

