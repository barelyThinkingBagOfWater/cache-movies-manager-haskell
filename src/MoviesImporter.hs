{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MoviesImporter (run2) where

import Control.Applicative
import Control.Monad
--import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import Servant
import Model

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import qualified Servant.Client.Streaming as S


type MoviesManagerAPI = "movies-manager" :> "movies" :> Get '[JSON] [Movie]

moviesManagerAPI :: Proxy MoviesManagerAPI
moviesManagerAPI = Proxy

importMovies :: ClientM [Movie]
importMovies = client moviesManagerAPI

-- (BaseUrl Http "172.18.42.4" 80)

-- https://www.servant.dev/client-in-5-minutes.html

run2 :: IO ()
run2 = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM importMovies (mkClientEnv manager' (BaseUrl Http "172.18.42.4" 80 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (movies) -> do
      print movies

