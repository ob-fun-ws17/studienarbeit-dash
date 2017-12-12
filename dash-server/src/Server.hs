{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Server
  ( startApp
  , app
  ) where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

startApp :: IO ()
startApp = run 8081 app

type API = "test" :> Get '[JSON] Test

data Test = Test
  { testString :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Test

server :: Server API
server = test
  where
    test :: Handler Test
    test = return (Test "It works!")

api :: Proxy API
api = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server
