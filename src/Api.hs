module Api where

import Servant
import Types

type Todos = "todos" :> Get '[JSON] [Todo]

type Todo = 
  "todo" 
    :> Capture "id" Int 
    :> Get '[JSON] (Maybe Todo)
  :<|>
    "todo" 
      :> Capture "id" Int
      :> Post '[JSON] PostNoContent

type TodoApi = Todos :<|> Todo

dummyTodos = [
    Todo 1 "Finish this tutorial" False,
    Todo 2 "Finish this tutorial again" False
  ]

server1 :: Server TodoApi
server1 = pure todosHandler

todosHandler :: Monad m => m [Todo]
todosHandler = return dummyTodos

getTodoHandler :: Int -> Maybe Todo
getTodoHandler id = dummyTodos !? id

todoApiProxy :: Proxy TodoApi
todoApiProxy = Proxy

app1 :: Application
app1 = serve todoApiProxy server1