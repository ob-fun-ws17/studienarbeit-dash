{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Task
Description : Everything for TaskAPI
Copyright   : (c) Benedikt Friedrich, 2017
License     : BSD-3
Maintainer  : Benedikt Friedrich
Stability   : experimental

This module contains the TaskAPI and its implementation.
-}
module Task
  ( TaskAPI
  , taskServer
  , concatDep
  , sortDep
  ) where

import           Data.Monoid
import           Data.String
import           Database.Persist.Sql
import           Entity
import           Servant
import           Types

-- |  The Rest API hadling tasks.
-- All its services' uri- paths start with "/task/"
type TaskAPI = "task" :>
  (    "add" :> ReqBody '[JSON] Task :> Get '[JSON] [DbDependency]
  :<|> "sort" :> Get '[JSON] [Int]
  )

-- | Implementation of the TaskAPI
taskServer :: ConnectionPool -- ^ the pool of database connections to be used
           -> Server TaskAPI -- ^ the server for the TaskAPI
taskServer pool = addTask
             :<|> sort
  where
    addTask :: Task -> Handler [DbDependency]
    addTask newTask = do
      let dependencyList = dependencies newTask
      let dependencyKeys = map (fromIntegral . depends) dependencyList
      allTaskKeys <- runDb pool $ selectKeysList ([] :: [Filter DbTask]) []
      let unsatisfiedDeps = filter (`notElem` map fromSqlKey allTaskKeys) dependencyKeys
      if not $ null unsatisfiedDeps
        then throwError err406 { errBody = fromString ("unsatisfiedDeps: " ++ show unsatisfiedDeps)}
        else do
          let dbTask = DbTask (name newTask)
          taskKey <- runDb pool $ insert dbTask
          runDb pool $ insertMany_ $
            map (\x -> DbDependency (toSqlKey $ fromIntegral $ depends x) taskKey (major x) (minor x)) dependencyList
          fmap (map entityVal) $ runDb pool $ selectList [DbDependencyChild ==. taskKey] []

    sort :: Handler [Int]
    sort = do
      depTuple <- loadDep pool
      taskEntityList <- runDb pool $ selectList ([] :: [Filter DbTask]) []
      return $ sortDep $ concatDep (map (fromIntegral . fromSqlKey . entityKey) taskEntityList) depTuple


loadDep :: ConnectionPool -> Handler [(Int,Int)]
loadDep pool =
  fmap ( map (depAsTuple . entityVal)) $ runDb pool $ selectList ([] :: [Filter DbDependency]) []
  where
    depAsTuple :: DbDependency -> (Int, Int)
    depAsTuple x = (fromIntegral $ fromSqlKey $ dbDependencyChild x, fromIntegral $ fromSqlKey $ dbDependencyParent x)

-- | Computes a list of dependencies
concatDep :: [Int] -- ^ list of tasks
          -> [(Int, Int)] -- ^ pairs of dependencies [(x, y) reads: x depends on y]
          -> [(Int, [Int])] -- ^ computed list [(x, ys) reads: x has ys dependencies]
concatDep t d =
  map (\x -> (x, map snd $ filter ((==) x . fst) d )) t

-- | Sorts dependencies
sortDep :: [(Int, [Int])] -- ^ list of tasks with coresponding dependencies
        -> [Int] -- ^ list of dependencies where n has to be used before n+1
sortDep [] = []
sortDep xs = withoutParent <> sortDep (map removeAlreadyListed withParent)
  where
    withoutParent :: [Int]
    withoutParent = map fst $ filter (null . snd) xs
    withParent :: [(Int,[Int])]
    withParent = filter (\(x,_) -> x `notElem` withoutParent) xs
    removeAlreadyListed :: (Int, [Int]) -> (Int, [Int])
    removeAlreadyListed (x, ys)= (x, filter (`elem` withoutParent) ys)
