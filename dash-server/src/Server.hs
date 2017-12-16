{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Server
  ( startApp
  , app
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)


import           Data.Aeson
import           GHC.Generics

import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
  testString String
  deriving Eq Read Show Generic
|]

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


instance ToJSON Test where
  toJSON (Test testString) =
    object ["testString" .= testString]


instance FromJSON Test where


server :: ConnectionPool -> Server API
server pool =
       test
  :<|> testAddJSON
  :<|> testAddParam
  :<|> testGetJSON

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
api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api $ server pool





-- import           Network.Wai
-- import           Network.Wai.Handler.Warp
-- import           Servant
--
-- type API = "test" :> Get '[JSON] Test
--
-- data Test = Test
--   { testString :: String
--   } deriving (Eq, Show, Generic)
--
-- instance ToJSON Test
--
-- server :: Server API
-- server = test
  -- where
  --   test :: Handler Test
  --   test = return (Test "It works!")
--
-- api :: Proxy API
-- api = Proxy
--
-- -- 'serve' comes from servant and hands you a WAI Application,
-- -- which you can think of as an "abstract" web application,
-- -- not yet a webserver.
-- app :: Application
-- app = serve api server
