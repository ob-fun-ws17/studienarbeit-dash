
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_HADDOCK prune #-}

{-|
Module      : Entity
Description : Persist entities
Copyright   : (c) Benedikt Friedrich, 2017
License     : BSD-3
Maintainer  : Benedikt Friedrich
Stability   : experimental

This module contains all the entities for the database.
The corresponding types are generated using TemplateHaskell and QuasiQuotes.
-}
module Entity where

import           Control.Monad.IO.Class
import           Data.Aeson.TH
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
DbTask
  name String
  deriving Eq Read Show Generic
DbDependency
  parent DbTaskId
  child DbTaskId
  UniqueD parent child
Todo
  context String
  status Status
  category CategoryId
  priority Priority
  deadline Day
  duration Int
  deriving Eq Read Show Generic
|]

deriveJSON defaultOptions ''Test
-- ^ FromJSON and ToJSON instances
deriveJSON defaultOptions ''DbDependency
-- ^ FromJSON and ToJSON instances
deriveJSON defaultOptions ''DbTask
-- ^ FromJSON and ToJSON instances
deriveJSON defaultOptions ''Todo
-- ^ FromJSON and ToJSON instances
deriveJSON defaultOptions ''Category
-- ^ FromJSON and ToJSON instances

-- | runs a database query
runDb :: ConnectionPool -- ^ the pool of database connections to use
      -> SqlPersistT IO b -- ^ the  database action to be executed
      -> Handler b -- ^ the Handler Monade for the corresponding Entity
runDb pool query = liftIO $ runSqlPool query pool
