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
  conn <- liftIO $ connect defaultConnectInfo
  runRedis conn $ set (C.pack (show (getMovieId movie))) $ (toStrict $ encode movie) --show for Int -> String, C.pack for String -> C.ByteString

saveMovies :: [Movie] -> IO ()
saveMovies movies = do
  mapM_ (saveMovie) movies

--getMovie :: Int -> Movie
--getMovie movieId = do
--  conn   <- liftIO $ connect defaultConnectInfo
--  movie <- runRedis conn $ get (show(movieId)) -- convert C.ByteString -> Movie
--  return movie