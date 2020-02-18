{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method

module Model (Movie, movies1) where

import Data.Aeson.TH


data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  } deriving (Show) -- for printing

$(deriveJSON defaultOptions 'Movie)
