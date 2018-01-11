module Main where

import           Network.Wai.Handler.Warp
import           Server

main :: IO ()
main = startApp "dash.db" 8080

-- | Starts the application with a SQLite database
startApp :: FilePath -- ^ the file the SQLite database will be saved in
         -> Int -- ^ the port on which the server will listen
         -> IO () -- ^ the IO Monad of the application
startApp file port =
  run port =<< initApp file
