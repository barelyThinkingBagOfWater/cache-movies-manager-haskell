{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method

module Model (Movie) where

import Data.Aeson.TH


data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  }

$(deriveJSON defaultOptions 'Movie)
