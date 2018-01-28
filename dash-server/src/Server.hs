{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Server
Description : Server based on Servant
Copyright   : (c) Benedikt Friedrich, 2017
License     : BSD-3
Maintainer  : Benedikt Friedrich
Stability   : experimental

This module controls the logging, initializes the database
and starts the Servant server.
-}
module Server
  ( app
  , initApp
  ) where

import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.String.Conversions

import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai

import           Entity
import           Servant
import           Task
import           Todo


-- | Initializes the Application with logging to StdErr and migrates the
-- database
initApp :: FilePath -- ^ the path for the database file
        -> IO Application
initApp file = do
  pool <- runStderrLoggingT $
    createSqlitePool (cs file) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

type API =
          "test" :> Get '[JSON] (Test)
          :<|> TodoAPI
          :<|> TaskAPI

server :: ConnectionPool -> Server API
server pool =
       test
  :<|> todoServer pool
  :<|> taskServer pool

  where
    test :: Handler Test
    test = return (Test "It works!")

api :: Proxy API
api = Proxy

-- | The executable Application
app :: ConnectionPool -- ^ the pool of database connections to be used
    -> Application -- ^ the application
app pool = serve api $ server pool
