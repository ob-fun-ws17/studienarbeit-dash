{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Task
  ( TaskAPI
  , taskServer
  ) where

import           Data.String
import qualified Data.Text            as Tx
import           Database.Persist.Sql
import           Entity
import           Servant
import           Types

type TaskAPI = "task" :>
  (    "add" :> ReqBody '[JSON] Task :> Get '[JSON] [Dependency]
  )

taskServer :: ConnectionPool -> Server TaskAPI
taskServer pool = addTask
  where
    addTask :: Task -> Handler [Dependency]
    addTask newTask = do
      let dependencyList = map fromIntegral $ dependencies newTask
      allTaskKeys <- runDb pool $ selectKeysList ([] :: [Filter DbTask]) []
      let unsatisfiedDeps = filter (`notElem` map fromSqlKey allTaskKeys) dependencyList
      if not $ null unsatisfiedDeps
        then throwError err406 { errBody = fromString ("unsatisfiedDeps: " ++ show unsatisfiedDeps)}
        else do
          let dbTask = DbTask (name newTask)
          taskKey <- runDb pool $ insert dbTask
          runDb pool $ insertMany_ $ map (\x -> Dependency (toSqlKey x) taskKey) dependencyList
          fmap (map entityVal) $ runDb pool $ selectList [DependencyChild ==. taskKey] []




loadAllTasks :: ConnectionPool -> Handler [DbTask]
loadAllTasks pool =  do
  allTasks <- runDb pool $ selectList [] []
  return $ fmap entityVal allTasks
