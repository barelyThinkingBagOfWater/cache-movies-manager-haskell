{-# LANGUAGE OverloadedStrings #-} -- for the command sent to the server, required?

module RedisConnector (saveMovies, printMovieIds) where

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
--   line above is fine? Try to get a saved movie still
--  runRedis conn $ set (movieId movie) (toStrict $ encode movie)

saveMovies :: [Movie] -> IO ()
saveMovies movies = do
  mapM_ (saveMovie) movies

-- help? https://stackoverflow.com/questions/15993496/yesod-how-to-send-redis-results-as-json

-- to update the entities : https://stackoverflow.com/questions/35610524/building-a-monad-on-top-of-hedis-a-haskell-redis-lib


printMovieId :: Movie -> IO ()
printMovieId movie = do
  print movie
--  putStrLn (title movie)


printMovieIds :: [Movie] -> IO ()
printMovieIds movies = do
  mapM_ (printMovieId) movies