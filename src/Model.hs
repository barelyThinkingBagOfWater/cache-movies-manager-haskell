{-# LANGUAGE DataKinds       #-} -- for [Json] and the String like the url
{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method
{-# LANGUAGE OverloadedStrings #-}

module Model (Movie, movies1) where

import Data.Aeson
import Data.Aeson.TH

data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  } deriving (Eq, Show) -- show for "print randomMovie"

$(deriveJSON defaultOptions 'Movie)


movies1 :: [Movie]
movies1 = [ Movie 1 "title1" "genres1" ["tag11", "tag12"]
         , Movie 2 "title2" "genres2" ["tag21", "tag22"]
          ]
