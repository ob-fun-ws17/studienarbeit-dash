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
        it "add with dependencies ok" $ do
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[]}")
            `shouldRespondWith` 200
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[]}")
            `shouldRespondWith` 200
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[1,2]}")
            `shouldRespondWith` "[{\"dependencyParent\":1,\"dependencyChild\":3},{\"dependencyParent\":2,\"dependencyChild\":3}]"
        it "add with dependencies not ok" $
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[1]}")
            `shouldRespondWith` "unsatisfiedDeps: [1]" {matchStatus = 406}


jsonHeader :: RequestHeaders
jsonHeader = [(hContentType , "application/json")]

withApp :: IO Application
withApp = inTempDirectory $ initApp "test.db"
