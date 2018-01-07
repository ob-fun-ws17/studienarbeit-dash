{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Task
  ( TaskAPI
  , taskServer
  ) where

import           Database.Persist.Sql
import           Servant

import           Entity

type TaskAPI = "task" :>
  (    "add" :> ReqBody '[JSON] Task :> Get '[JSON] (Key Task)
  )

taskServer :: ConnectionPool -> Server TaskAPI
taskServer pool = addTask
  where
    addTask :: Task -> Handler (Key Task)
    addTask x = runDb pool $ insert x

loadAllTasks :: ConnectionPool -> Handler [Task]
loadAllTasks pool =  do
  allTasks <- runDb pool $ selectList [] []
  return $ fmap entityVal allTasks
