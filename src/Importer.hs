{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE TypeOperators   #-} -- for the :>
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Importer
    (
--    importAllMovies
    ) where

--import Data.Aeson
--import Data.Proxy
--import GHC.Generics
--import Network.HTTP.Client (newManager, defaultManagerSettings)
--import Servant.API
--import Servant.Client
--import Servant.Types.SourceT (foreach)
--
--import qualified Servant.Client.Streaming as S


data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  }

--
--baseUrl :: BaseUrl
--baseUrl = BaseUrl Http "172.18.42.4" 80 "/movies-manager/movies"


-- https://www.servant.dev/client-in-5-minutes.html