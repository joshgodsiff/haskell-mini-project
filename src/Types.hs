{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

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