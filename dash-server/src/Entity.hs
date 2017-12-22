{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Entity where


import           Data.Aeson.TH
import           Data.Text           as Text
import           Data.Time
import           Database.Persist.TH
import           GHC.Generics

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
