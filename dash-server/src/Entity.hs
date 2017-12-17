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
import           Database.Persist.TH
import           GHC.Generics

import           Types

share [mkPersist sqlSettings {mpsPrefixFields = False}, mkMigrate "migrateAll"] [persistLowerCase|
Test
  testString String
  deriving Eq Read Show Generic
Todo
  context Text
  status String
  category String
  priority Priority
  deriving Eq Read Show Generic
|]

Prelude.concat <$> mapM (deriveJSON defaultOptions) [''Test, ''Todo]
