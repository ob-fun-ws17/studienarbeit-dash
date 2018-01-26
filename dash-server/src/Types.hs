{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-|
Module      : Types
Description : Custom database types and JSON for the API
Copyright   : (c) Benedikt Friedrich, 2017
License     : BSD-3
Maintainer  : Benedikt Friedrich
Stability   : experimental

This module contains the custom database types which for some reason can't
be in the same file as the database entities.
Also there are the types to derive ToJSON and FromJSON which are only used
for the API and not as database entities.
-}
module Types where

import           Data.Aeson.TH

import           Database.Persist.TH
import           GHC.Generics

-- | Status of a Todo
data Status = Open -- ^ not yet started
            | InProgress -- ^ started but not finished
            | Closed -- ^ finished
  deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Status"
-- ^ PersistFieldSql instances for Status
deriveJSON defaultOptions ''Status
-- ^ FromJSON and ToJSON instances for Status

-- | Priority of a Todo
data Priority = Low -- ^ Low Priority
              | Middle -- ^ Medium Priority
              | High -- ^ High Priority
  deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Priority"
-- ^ PersistFieldSql instances for Priority
deriveJSON defaultOptions ''Priority
-- ^ FromJSON and ToJSON instances for Priority

-- | A Dependency element to be used as JSON for the TaskAPI
data Dependency = Dependency
  { depends :: Int -- ^ the key of the task to depend on
  , major   :: Int -- ^ major release number
  , minor   :: Int -- ^ mionr release number
  } deriving (Show, Read, Eq, Ord, Generic)
deriveJSON defaultOptions ''Dependency
-- ^ FromJSON and ToJSON instances for Dependency

-- | A Task element to be used as JSON for the TaskAPI
data Task = Task
  { name         :: String -- ^ the name of the task
  , dependencies :: [Dependency] --dependencies of the task
  } deriving (Show, Read, Eq, Ord, Generic)
deriveJSON defaultOptions ''Task
-- ^ FromJSON and ToJSON instances for Task
