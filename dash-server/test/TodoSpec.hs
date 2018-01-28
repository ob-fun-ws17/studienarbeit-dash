{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TodoSpec (spec) where

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
  with withApp $
    describe "/todo" $ do
      it "ADD category" addTestCategory
      it "ADD and REMOVE Todo" $ do
        addTestCategory
        request methodGet "/todo/add" jsonHeader "{\"todoContext\":\"Context\",\"todoStatus\":\"Open\",\"todoCategory\":1,\"todoPriority\":\"High\",\"todoDeadline\":\"2017-11-17\",\"todoDuration\":1}"
          `shouldRespondWith` "1"
        get "/todo/1" `shouldRespondWith`
          "{\"todoContext\":\"Context\",\"todoStatus\":\"Open\",\"todoCategory\":1,\"todoPriority\":\"High\",\"todoDeadline\":\"2017-11-17\",\"todoDuration\":1}"
        get "/todo/remove/1" `shouldRespondWith` 200
      it "GET todo" $
          get "/todo" `shouldRespondWith` "[]"
      it "return only over deadline" $ do
        addTestCategory
        today <- liftIO getToday
        yesterday <- liftIO $ fmap (addDays (-1)) getToday
        request methodGet "/todo/add" jsonHeader (pack (testTodoOfDay yesterday))
          `shouldRespondWith` "1"
        request methodGet "/todo/add" jsonHeader (pack (testTodoOfDay today))
          `shouldRespondWith` "2"
        get "/todo/check" `shouldRespondWith` fromString ("[{\"todoContext\":\"Context\",\"todoStatus\":\"Open\",\"todoCategory\":1,\"todoPriority\":\"High\",\"todoDeadline\":\""
          ++ showGregorian yesterday ++ "\",\"todoDuration\":1}]")
  describe "checkTodoDeadline" $
    it "no Todos" $ do
      today <- liftIO getToday
      checkTodoDeadline' [] today `shouldBe` []

addTestCategory :: WaiExpectation
addTestCategory = post "/todo/addCategory/?category=Job" "" `shouldRespondWith` "1"

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
