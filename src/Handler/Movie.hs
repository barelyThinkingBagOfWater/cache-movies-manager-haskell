module Handler.Movie where

import Import


getMovieR :: String -> Handler RepHtml
getMovieR theText = do
              defaultLayout $ do
                  <h1>hello</h1>