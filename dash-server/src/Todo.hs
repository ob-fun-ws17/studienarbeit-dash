{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Todo
  ( TodoAPI
  , todoServer
  , checkTodoDeadline'
  , getToday
  ) where

import           Control.Monad.IO.Class
import           Data.Time
import           Database.Persist.Sql
import           Servant

import           Entity

type TodoAPI =
       Get '[JSON] [Todo]
  :<|> Capture "id" TodoId :>  Get '[JSON] Todo
  :<|> "check" :> Get '[JSON] [Todo]

todoServer :: ConnectionPool -> Server TodoAPI
todoServer pool = getAllTodos
             :<|> getTodo
             :<|> checkTodoDeadline
  where
    getAllTodos :: Handler [Todo]
    getAllTodos = loadAllTodos pool

    getTodo :: TodoId -> Handler Todo
    getTodo todoId = do
      maybeTodo <- runDb pool $ get todoId
      case maybeTodo of
        Just a  -> return a
        Nothing -> throwError err404

    checkTodoDeadline :: Handler [Todo]
    checkTodoDeadline = do
      todoList <- loadAllTodos pool
      today <- liftIO getToday
      return $ checkTodoDeadline' todoList today

checkTodoDeadline' :: [Todo] -> Day -> [Todo]
checkTodoDeadline' allTodos today =
  filter (\x -> today > todoDeadline x) allTodos

getToday :: IO Day
getToday =  fmap utctDay getCurrentTime

loadAllTodos :: ConnectionPool -> Handler [Todo]
loadAllTodos pool =  do
  allTodos <- runDb pool $ selectList [] []
  return $ fmap entityVal allTodos
