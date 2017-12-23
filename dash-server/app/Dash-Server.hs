module Main where

import           Server

main :: IO ()
main = startApp "dash.db" 8080
