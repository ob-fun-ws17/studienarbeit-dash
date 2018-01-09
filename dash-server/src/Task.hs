{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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

type TaskAPI = "task" :>
  (    "add" :> ReqBody '[JSON] Task :> Get '[JSON] [Dependency]
  :<|> "sort" :> Get '[JSON] [Int]
  )

taskServer :: ConnectionPool -> Server TaskAPI
taskServer pool = addTask
             :<|> sort
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

    sort :: Handler [Int]
    sort = do
      depTuple <- loadDep pool
      taskEntityList <- runDb pool $ selectList ([] :: [Filter DbTask]) []
      return $ sortDep $ concatDep (map (fromIntegral . fromSqlKey . entityKey) taskEntityList) depTuple


loadDep :: ConnectionPool -> Handler [(Int,Int)]
loadDep pool =
  fmap ( map (depAsTuple . entityVal)) $ runDb pool $ selectList ([] :: [Filter Dependency]) []
  where
    depAsTuple :: Dependency -> (Int, Int)
    depAsTuple x = (fromIntegral $ fromSqlKey $ dependencyChild x, fromIntegral $ fromSqlKey $ dependencyParent x)

concatDep :: [Int] -> [(Int, Int)] -> [(Int, [Int])]
concatDep t d =
  map (\x -> (x, map snd $ filter ((==) x . fst) d )) t

sortDep :: [(Int, [Int])] -> [Int]
sortDep [] = []
sortDep xs = withoutChild <> sortDep (map removeAlreadyListed withParent)
  where
    withoutChild :: [Int]
    withoutChild = map fst $ filter (null . snd) xs
    withParent :: [(Int,[Int])]
    withParent = filter (\(x,_) -> x `notElem` withoutChild) xs
    removeAlreadyListed :: (Int, [Int]) -> (Int, [Int])
    removeAlreadyListed (x, ys)= (x, filter (`elem` withoutChild) ys)