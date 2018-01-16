{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TaskSpec (spec) where

import           Data.ByteString.Lazy.Char8
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Servant
import           Server                     (initApp)
import           Task                       (concatDep, sortDep)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Mockery.Directory

spec :: Spec
spec = do
  with withApp $
    describe "/task" $ do
        it "add with dependencies ok" $ do
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[]}")
            `shouldRespondWith` 200
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[]}")
            `shouldRespondWith` 200
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[{\"depends\":1,\"major\":1,\"minor\":0},{\"depends\":2,\"major\":1,\"minor\":0}]}")
            `shouldRespondWith` "[{\"dbDependencyParent\":1,\"dbDependencyChild\":3,\"dbDependencyMajor\":1,\"dbDependencyMinor\":0},{\"dbDependencyParent\":2,\"dbDependencyChild\":3,\"dbDependencyMajor\":1,\"dbDependencyMinor\":0}]"
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[{\"depends\":1,\"major\":1,\"minor\":0},{\"depends\":3,\"major\":1,\"minor\":0}]}")
            `shouldRespondWith` "[{\"dbDependencyParent\":1,\"dbDependencyChild\":4,\"dbDependencyMajor\":1,\"dbDependencyMinor\":0},{\"dbDependencyParent\":3,\"dbDependencyChild\":4,\"dbDependencyMajor\":1,\"dbDependencyMinor\":0}]"
        it "add with dependencies not ok" $
          request methodGet "/task/add" jsonHeader (pack "{\"name\":\"a\",\"dependencies\":[{\"depends\":1,\"major\":1,\"minor\":0}]}")
            `shouldRespondWith` "unsatisfiedDeps: [1]" {matchStatus = 406}
  describe "concatDep" $ do
    it "on empty" $
      concatDep [1,2] []  `shouldBe` [(1, []), (2, [])]
    it "no dependencies for one" $
      concatDep [1,2] [(2,1)] `shouldBe` [(1, []), (2, [1])]
  describe "sortDep" $ do
    it "empty dependencies" $
      sortDep [(1,[]),(2,[])] `shouldBe` [1,2]
    it "with dependencies" $
      sortDep [(1,[]), (2,[1]), (3,[1,2]), (4,[1,3])] `shouldBe` [1,2,3,4]
  describe "combination of concat and sort" $
    it "" $
      sortDep (concatDep [1,2,3] [(2,1),(3,1),(3,2)]) `shouldBe` [1,2,3]

jsonHeader :: RequestHeaders
jsonHeader = [(hContentType , "application/json")]

withApp :: IO Application
withApp = inTempDirectory $ initApp "test.db"
