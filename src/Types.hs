{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Types where

import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple (field, FromRow (..), ToRow (..))

import qualified Data.Text as T

-- Todo list types
data Todo 
  = Todo
  { todoId :: Int
  , todoTitle :: T.Text
  , todoCompleted :: Bool
  } deriving (Show, Generic, Eq)

instance ToJSON Todo
instance FromJSON Todo

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

instance ToRow Todo where
  toRow (Todo id' title completed) = toRow (id', title, completed)
