
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Entity where

import           Control.Monad.IO.Class
import           Data.Aeson.TH
import           Data.Text              as Text
import           Data.Time
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics
import           Servant

import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Test
  testString String
  deriving Eq Read Show Generic
Category
  category String
  Unique CategoryU category
  deriving Eq Read Show Generic
Todo
  context Text
  status Status
  category CategoryId
  priority Priority
  deadline Day
  duration Int
  deriving Eq Read Show Generic
|]

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''Test, ''Todo, ''Category]

runDb :: ConnectionPool -> SqlPersistT IO b -> Handler b
runDb pool query = liftIO $ runSqlPool query pool
