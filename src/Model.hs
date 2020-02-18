{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method

module Model (Movie) where

import Data.Aeson.TH


data Movie = Movie
  { movieId :: String
  , title :: String
  , genres :: String
  , tags :: [String]
  } deriving (Show) -- for printing

$(deriveJSON defaultOptions 'Movie)
