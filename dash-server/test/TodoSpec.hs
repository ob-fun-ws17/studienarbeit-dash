{-# LANGUAGE OverloadedStrings #-}

module TodoSpec (spec) where

-- import           Control.Exception          (ErrorCall (..), throwIO)
-- import           Control.Monad.Catch
-- import           Control.Monad.Trans.Except
--
-- import           Network.HTTP.Client
-- import           Network.Wai.Handler.Warp
--
-- import           Servant.API
-- import           Servant.Client

import           Servant
import           Server                 (initApp)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Mockery.Directory


spec :: Spec
spec = with (withApp) $
    describe "/todo" $ do

        it "GET todo" $
            get "/todo" `shouldRespondWith` "[]"
        it "ADD category" $ do
          post "/addCategory/?category=Job" "" `shouldRespondWith` "1"
--         it "ADD todo" $ do
--           post "/todo" "{\"todoContext\":\"Text\",\"todoStatus\":\"Open\",\"todoCategory\":1,\"todoPriority\":\"High\",\"todoDeadline\":\"2017-12-12\",\"todoDuration\":1,\"todoCreated\":null}"
--               `shouldRespondWith` "1"
--         it "GET todo" $
--             get "/todo" `shouldRespondWith` "[]"

withApp :: IO Application
withApp = inTempDirectory $ initApp "test.db"
--
-- try :: Int -> (Manager -> BaseUrl -> ClientM a) -> IO a
-- try port action = do
--   manager <- newManager defaultManagerSettings
--   let baseUrl = BaseUrl Http "localhost" port ""
--   throwM (action manager baseUrl)
