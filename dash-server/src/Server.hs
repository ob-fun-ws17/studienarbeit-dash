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
             :<|> "testAddJSON" :> ReqBody '[JSON] Test :> Post '[JSON] (Key Test)
             :<|> "testAddParam" :> QueryParam "param" String :> Post '[JSON] (String)
             :<|> "testGetJSON" :> QueryParam "param" String :> Post '[JSON] (Test)
             :<|> TodoAPI
             :<|> TaskAPI

server :: ConnectionPool -> Server API
server pool =
       test
  :<|> testAddJSON
  :<|> testAddParam
  :<|> testGetJSON
  :<|> todoServer pool
  :<|> taskServer pool

  where
    test :: Handler Test
    test = return (Test "It works!")

    testAddJSON :: Test -> Handler (Key Test)
    testAddJSON testJson = runDb pool $ insert testJson

    testAddParam :: Maybe String -> Handler String
    testAddParam testParam =
      case testParam of
        Just a  -> fmap (show . fromSqlKey ) (runDb pool (insert $ Test a))
        Nothing -> throwError err404
  --
    testGetJSON :: Maybe String -> Handler (Test)
    testGetJSON param =
      case param of
        Just a -> do
          maybeTest <- runDb pool (selectFirst [TestTestString ==. a] [])
          case maybeTest of
            Just test -> return $ entityVal test
            Nothing   -> throwError err404

        Nothing -> throwError err404

api :: Proxy API
api = Proxy

-- | The executable Application
app :: ConnectionPool -- ^ the pool of database connections to be used
    -> Application -- ^ the application
app pool = serve api $ server pool
