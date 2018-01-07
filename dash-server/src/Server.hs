{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Server
  ( startApp
  , initApp
  , app
  ) where

import           Control.Monad.Logger     (runStderrLoggingT)

import           Data.String.Conversions
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           Entity
import           Todo

startApp :: FilePath -> Int -> IO ()
startApp file port =
  run port =<< initApp file

initApp :: FilePath -> IO Application
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
             :<|> "addTodo" :> ReqBody '[JSON] Todo :> Get '[JSON] (Key Todo)
             :<|> "addCategory" :> QueryParam "category" String :> Post '[JSON] (Key Category)
             :<|> "todo" :> TodoAPI

server :: ConnectionPool -> Server API
server pool =
       test
  :<|> testAddJSON
  :<|> testAddParam
  :<|> testGetJSON
  :<|> addTodo
  :<|> addCategory
  :<|> todoServer pool

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

    addTodo :: Todo -> Handler (Key Todo)
    addTodo x = runDb pool $ insert x

    addCategory :: Maybe String -> Handler (Key Category)
    addCategory param =
      case param of
        Just a  -> runDb pool $ insert $ Category a
        Nothing -> throwError err404

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api $ server pool
