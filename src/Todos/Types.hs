{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todos.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- Todo data type
data Todo = Todo
    { todoId :: Int
    , todoTitle :: Text
    , todoCompleted :: Bool
    , timestamp :: UTCTime
    }
    deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

-- DTO
newtype NewTodoDto = NewTodoDto
    { newTodoTitle :: Text
    }
    deriving (Generic)

instance ToJSON NewTodoDto
instance FromJSON NewTodoDto

data UpdateTodoDto = UpdateTodoDto
    { updateTodoCompleted :: Maybe Bool
    }
    deriving (Generic)

instance ToJSON UpdateTodoDto
instance FromJSON UpdateTodoDto
