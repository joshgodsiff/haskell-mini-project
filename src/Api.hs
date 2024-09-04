{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import Servant
import Types
import Db (openDb, readAllTodos, readOneTodo, readOneTodo)
import Control.Monad.IO.Class (liftIO)

type Todos = "todos" :> Get '[JSON] [Todo]

type TodoGet = 
  "todo"
    :> Capture "id" Int 
    :> Get '[JSON] (Maybe Todo)
  

type TodoPost = 
  "todo" 
    :> Capture "id" Int
    :> ReqBody '[JSON] Todo
    :> PostNoContent

type TodosApi = Todos :<|> TodoGet

dummyTodos :: [Todo]
dummyTodos = [
    Todo 1 "Finish this tutorial" False,
    Todo 2 "Finish this tutorial again" False
  ]

server1 :: Server TodosApi
server1
  =    todosHandler
  :<|> todoGetHandler

todosHandler :: Handler [Todo]
todosHandler = liftIO $ do
  db <- openDb
  readAllTodos db

todoGetHandler :: Int -> Handler (Maybe Todo)
todoGetHandler id' = liftIO $ do
  db <- openDb
  readOneTodo db id'

todoPutHandler :: Int -> Todo -> Handler PostNoContent
todoPutHandler _ = error ""  -- dummy implementation

(!?) :: [a] -> Int -> Maybe a
{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

infixl 9  !?

todoApiProxy :: Proxy TodosApi
todoApiProxy = Proxy

app1 :: Application
app1 = serve todoApiProxy server1
