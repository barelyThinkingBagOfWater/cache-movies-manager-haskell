{-# LANGUAGE TemplateHaskell #-} -- for the deriveJson method

module Model (Movie, getMovieId) where

import Data.Aeson.TH


data Movie = Movie
  { movieId :: Int
  , title :: String
  , genres :: String
  , tags :: [String]
  } deriving (Show) -- for printing

$(deriveJSON defaultOptions 'Movie)

getMovieId :: Movie -> Int -- should be created by default? Why 'movieId movie' doesn't work elsewhere?
getMovieId movie = movieId movie

