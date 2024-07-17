{-# LANGUAGE OverloadedStrings #-}

module Db where

import Data.Maybe (listToMaybe)
import qualified Database.SQLite.Simple as Sql -- Now Sql.whatever
import Database.SQLite.Simple (Connection)
import Types


openDb :: IO Connection
openDb = Sql.open "todos.db"

insertTodo :: Connection -> Todo -> IO ()
insertTodo conn t = do
  Sql.execute conn "INSERT INTO todos (id, title, completed) VALUES (?, ?, ?)" t
  
readAllTodos :: Connection -> IO [Todo]
readAllTodos conn = 
  Sql.query conn "SELECT id, title, completed FROM todos" ()

readOneTodo :: Connection -> Int -> IO (Maybe Todo)
readOneTodo conn id_ = do
  result <- Sql.query conn "SELECT id, title, completed FROM todos WHERE id=?" $ Sql.Only id_
  pure $ listToMaybe result
