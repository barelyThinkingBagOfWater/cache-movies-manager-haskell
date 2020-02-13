{-# LANGUAGE OverloadedStrings #-} -- for the command sent to the server, required?

module RedisConnector (saveMovies) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, toJSON)

import qualified Data.Text                  as  T
import qualified Data.ByteString.Char8      as  C
import           Data.ByteString.Lazy       (toStrict)

import Database.Redis
import Model


saveMovie :: Movie -> IO (Either Reply Status)
saveMovie movie = do
  conn   <- liftIO $ connect defaultConnectInfo
  runRedis conn $ set (C.pack "json_response") $ (toStrict $ encode movie)
--  runRedis conn $ set movie.movieId movie.toJSON

saveMovies :: [Movie] -> [IO (Either Reply Status)]
saveMovies movies = do
  map (saveMovie) movies



