{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todos.Api where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.HTML.Blaze ()
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html)
import Web.FormUrlEncoded (FromForm)

data HTML = HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Html where
    mimeRender _ = renderHtml

-- Form Data
newtype CreateTodoForm = CreateTodoForm
    { createTitle :: Text
    }
    deriving (Show, Generic)

instance FromForm CreateTodoForm

newtype UpdateTodoForm = UpdateTodoForm
    { updateCompleted :: Maybe Bool
    }
    deriving (Show, Generic)

instance FromForm UpdateTodoForm

-- API Endpoints
type Root =
    Get '[HTML] Html

type GetTodos =
    Get '[HTML] Html

type GetTodo =
    Capture "todoId" Int :> Get '[HTML] Html

type PostTodo =
    ReqBody '[FormUrlEncoded] CreateTodoForm
        :> Post '[HTML] Html

type PutTodo =
    Capture "todoId" Int
        :> ReqBody '[FormUrlEncoded] UpdateTodoForm
        :> Put '[HTML] Html

type DeleteTodoAction =
    Capture "todoId" Int
        :> Delete '[HTML] Html

type TodoAPI =
    "todos"
        :> ( GetTodos
                :<|> PostTodo
                :<|> GetTodo
                :<|> PutTodo
                :<|> DeleteTodoAction
           )

type API =
    Root :<|> TodoAPI
