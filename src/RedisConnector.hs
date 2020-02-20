{-# LANGUAGE OverloadedStrings #-} -- for the command sent to the server, required?

module RedisConnector (saveMovies, getMovie) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, decode)

import qualified Data.Text                  as  T
import qualified Data.ByteString.Char8      as  C
import           Data.ByteString.Lazy       (toStrict)

import Database.Redis
import Model

-- for config, read env variables, https://hackage.haskell.org/package/envy
saveMovie :: Movie -> IO (Either Reply Status)
saveMovie movie = do
  conn <- liftIO $ connect defaultConnectInfo --you could try to add more max concurrent connections, check object ConnectInfo
  runRedis conn $ set (C.pack (show (getMovieId movie))) $ (toStrict $ encode movie) --show for Int -> String, C.pack for String -> C.ByteString

saveMovies :: [Movie] -> IO ()
saveMovies movies = do
  mapM_ (saveMovie) movies

getMovie :: Int -> IO (Either Reply (Maybe C.ByteString)) -- NOT TESTED
getMovie movieId = do
  conn <- liftIO $ connect defaultConnectInfo
  encodedMovie <- runRedis conn $ get (C.pack (show (movieId))) -- now convert C.ByteString -> Movie
  return encodedMovie

-- connection lost est pas du à la limite des 100k avec redis? If you consume a stream one by one
-- that should go better with Redis as well

-- https://hackage.haskell.org/package/hedis-0.12.11/docs/Database-Redis.html
-- Connection to the server lost:
   --  In case of a lost connection, command functions throw a ConnectionLostException.
   --  It can only be caught outside of runRedis.
   --  Hardcore : https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
