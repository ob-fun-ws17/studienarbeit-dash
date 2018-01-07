{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaskSpec (spec) where

import           Data.ByteString.Lazy.Char8
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Servant
import           Server                     (initApp)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Mockery.Directory

spec :: Spec
spec = do

  with (withApp) $ do

    describe "/task" $ do
        it "return only over deadline" $ do
          request methodGet "/task/add" jsonHeader (pack "{\"taskName\":\"a\"}")
            `shouldRespondWith` "1"
          request methodGet "/task/add" jsonHeader (pack "{\"taskName\":\"b\"}")
            `shouldRespondWith` "2"

jsonHeader :: RequestHeaders
jsonHeader = [(hContentType , "application/json")]

withApp :: IO Application
withApp = inTempDirectory $ initApp "test.db"
