{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Server
  ( startApp
  , app
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)

import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           Entity


startApp :: Int -> IO ()
startApp port = do
  pool <- runStderrLoggingT $
    createSqlitePool "dash.db" 5

  runSqlPool (runMigration migrateAll) pool
  run port $ app pool


runDb :: SqlPersistT IO b -> ConnectionPool -> Handler b
runDb query pool = liftIO $ runSqlPool query pool


type API =
             "test" :> Get '[JSON] (Test)
             :<|> "testAddJSON" :> ReqBody '[JSON] Test :> Post '[JSON] (Key Test)
             :<|> "testAddParam" :> QueryParam "param" String :> Post '[JSON] (String)
             :<|> "testGetJSON" :> QueryParam "param" String :> Post '[JSON] (Test)
             :<|> "addTodo" :> ReqBody '[JSON] Todo :> Post '[JSON] (Key Todo)
             :<|> "addCategory" :> QueryParam "category" String :> Post '[JSON] (Key Category)
server :: ConnectionPool -> Server API
server pool =
       test
  :<|> testAddJSON
  :<|> testAddParam
  :<|> testGetJSON
  :<|> addTodo
  :<|> addCategory

  where
    test :: Handler Test
    test = return (Test "It works!")

    testAddJSON :: Test -> Handler (Key Test)
    testAddJSON testJson = runDb (insert testJson) pool

    testAddParam :: Maybe String -> Handler String
    testAddParam testParam =
      case testParam of
        Just a  -> fmap (show . fromSqlKey ) (runDb  (insert (Test a))  pool)
        Nothing -> throwError err404
  --
    testGetJSON :: Maybe String -> Handler (Test)
    testGetJSON param =
      case param of
        Just a -> do
          maybeTest <- runDb (selectFirst [TestTestString ==. a] []) pool
          case maybeTest of
            Just test -> return $ entityVal test
            Nothing   -> throwError err404

        Nothing -> throwError err404

    addTodo :: Todo -> Handler (Key Todo)
    addTodo x = runDb (insert x)  pool

    addCategory :: Maybe String -> Handler (Key Category)
    addCategory param =
      case param of
        Just a  -> runDb (insert (Category a)) pool
        Nothing -> throwError err404

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api $ server pool
