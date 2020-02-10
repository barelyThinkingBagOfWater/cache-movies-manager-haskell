{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MoviesImporter where

import Control.Applicative
import Control.Monad
--import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.Client
import Servant

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import qualified Servant.Client.Streaming as S


data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions 'Movie)


type MoviesManagerAPI = "movies-manager" :> "movies" :> Get '[JSON] [Movie]

proxyAPI :: Proxy MoviesManagerAPI
proxyAPI = Proxy

importMovies :: IO [Movie]
importMovies = client proxyAPI (BaseUrl Http "172.18.42.4" 80)

-- https://www.servant.dev/client-in-5-minutes.html