{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where

import           Data.Aeson.TH

import           Database.Persist.TH
import           GHC.Generics

data Status = Open | InProgress | Closed
 deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Status"

data Priority = Low | Middle | High
  deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Priority"

data Task = Task
  { name         :: String
  , dependencies :: [Int]
  } deriving (Show, Read, Eq, Ord, Generic)

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''Task, ''Priority, ''Status]
