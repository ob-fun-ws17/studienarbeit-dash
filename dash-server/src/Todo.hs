{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Todo
Description : Everything for TodoAPI
Copyright   : (c) Benedikt Friedrich, 2017
License     : BSD-3
Maintainer  : Benedikt Friedrich
Stability   : experimental


This module contains the TodoAPI and its implementation.
-}
module Todo
  ( TodoAPI
  , todoServer
  , checkTodoDeadline'
  , getToday
  ) where

import           Control.Monad.IO.Class
import           Data.Int
import           Data.Maybe
import           Data.Time
import           Database.Persist.Sql
import           Servant

import           Entity

-- |  The Rest API hadling todos.
-- All its services' uri- paths start with "/todo/"
type TodoAPI = "todo" :>
  (     Get '[JSON] [Todo]
  :<|> "add" :> ReqBody '[JSON] Todo :> Get '[JSON] (Key Todo)
  :<|> Capture "id" Int64 :>  Get '[JSON] Todo
  :<|> "remove" :> Capture "id" Int64 :> Get '[JSON] ()
  :<|> "check" :> Get '[JSON] [Todo]
  )

-- | Implementation of the TodoAPI
todoServer :: ConnectionPool -- ^ the pool of database connections to be used
           -> Server TodoAPI -- ^ the server for the TodoAPI
todoServer pool = getAllTodos
             :<|> addTodo
             :<|> getTodo
             :<|> removeTodo
             :<|> checkTodoDeadline
  where
    getAllTodos :: Handler [Todo]
    getAllTodos = loadAllTodos pool

    addTodo :: Todo -> Handler (Key Todo)
    addTodo x = runDb pool $ insert x

    getTodo :: Int64 -> Handler Todo
    getTodo todoId = do
      maybeTodo <- runDb pool $ get $ toSqlKey todoId
      case maybeTodo of
        Just a  -> return a
        Nothing -> throwError err404

    removeTodo :: Int64 -> Handler ()
    removeTodo todoId = do
      maybeTodo <- runDb pool $ get (toSqlKey todoId:: (Key Todo))
      if isJust maybeTodo
        then runDb pool $ delete (toSqlKey todoId:: (Key Todo))
        else throwError err404

    checkTodoDeadline :: Handler [Todo]
    checkTodoDeadline = do
      todoList <- loadAllTodos pool
      today <- liftIO getToday
      return $ checkTodoDeadline' todoList today

-- | Checks for all given Todos whether or not there is enought time left
-- for them to be compleated
-- TODO: use duration
checkTodoDeadline' :: [Todo] -- ^ List of Todos to check
                   -> Day -- ^ the day on wich the todos should be finished
                   -> [Todo] -- ^ list of todos with not enought time left
checkTodoDeadline' allTodos day =
  filter (\x -> day > todoDeadline x) allTodos

-- | Returns today as an IO Monad of Day
getToday :: IO Day -- ^ the Monad of today
getToday =  fmap utctDay getCurrentTime

loadAllTodos :: ConnectionPool -> Handler [Todo]
loadAllTodos pool =  do
  allTodos <- runDb pool $ selectList [] []
  return $ fmap entityVal allTodos
