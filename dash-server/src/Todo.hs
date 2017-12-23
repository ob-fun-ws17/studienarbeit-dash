{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Todo
  ( TodoAPI
  , todoServer
  ) where

import           Database.Persist.Sql

import           Servant

import           Entity

type TodoAPI =
       Get '[JSON] [Todo]
  :<|> Capture "id" TodoId :>  Get '[JSON] Todo

todoServer :: ConnectionPool -> Server TodoAPI
todoServer pool = getAllTodos
             :<|> getTodo
  where
    getAllTodos :: Handler [Todo]
    getAllTodos = do
      allTodos :: [Entity Todo] <- runDb pool $ selectList [] []
      return $ fmap entityVal allTodos

    getTodo :: TodoId -> Handler Todo
    getTodo todoId = do
      maybeTodo <- runDb pool $ get todoId
      case maybeTodo of
        Just a  -> return a
        Nothing -> throwError err404
