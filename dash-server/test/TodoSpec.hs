{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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


import           Data.ByteString.Lazy.Char8
import           Data.String
import           Data.Time
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Servant
import           Server                     (initApp)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Mockery.Directory
import           Todo                       (checkTodoDeadline', getToday)


spec :: Spec
spec = do


  with (withApp) $ do

    describe "/todo" $ do
        it "GET todo" $
            get "/todo" `shouldRespondWith` "[]"
        it "ADD category" $ do
          post "/addCategory/?category=Job" "" `shouldRespondWith` "1"
        it "return only over deadline" $ do
          post "/addCategory/?category=Job" "" `shouldRespondWith` "1"
          today <- liftIO getToday
          yesterday <- liftIO $ fmap (addDays (-1)) getToday
          request methodGet "/addTodo" jsonHeader (pack (testTodoOfDay yesterday))
            `shouldRespondWith` "1"
          request methodGet "/addTodo" jsonHeader (pack (testTodoOfDay today))
            `shouldRespondWith` "2"
          get "/todo/check" `shouldRespondWith` fromString ("[{\"todoContext\":\"Context\",\"todoStatus\":\"Open\",\"todoCategory\":1,\"todoPriority\":\"High\",\"todoDeadline\":\""
            ++ showGregorian yesterday ++ "\",\"todoDuration\":1}]")

  describe "checkTodoDeadline" $ do
    it "no Todos" $ do
      today <- liftIO getToday
      checkTodoDeadline' [] today `shouldBe` []



testTodoOfDay :: Day -> String
testTodoOfDay day = "{\"todoContext\":\"Context\",\"todoStatus\":\"Open\",\"todoCategory\":1,\"todoPriority\":\"High\",\"todoDeadline\":\""
               ++ showGregorian day ++ "\",\"todoDuration\":1}"

jsonHeader :: RequestHeaders
jsonHeader = [(hContentType , "application/json")]

withApp :: IO Application
withApp = inTempDirectory $ initApp "test.db"
--
-- try :: Int -> (Manager -> BaseUrl -> ClientM a) -> IO a
-- try port action = do
--   manager <- newManager defaultManagerSettings
--   let baseUrl = BaseUrl Http "localhost" port ""
--   throwM (action manager baseUrl)
