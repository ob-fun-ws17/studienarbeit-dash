{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import           Server         (app)
import           Test.Hspec
import           Test.Hspec.Wai

spec :: Spec
spec = with (return $ app undefined) $
    describe "GET /test" $ do
        it "200" $
            get "/test" `shouldRespondWith` 200
        it "check response" $
            get "/test" `shouldRespondWith` "{\"testString\":\"It works!\"}"
