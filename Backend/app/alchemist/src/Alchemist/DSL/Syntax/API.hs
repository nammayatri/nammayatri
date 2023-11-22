module Alchemist.DSL.Syntax.API where

import Kernel.Prelude

data APIEndpoint = APIEndpoint
  { method :: HttpMethod,
    path :: String,
    auth :: Maybe AuthType,
    headers :: [Header],
    requestType :: Maybe DataType,
    responseType :: DataType
  }
  deriving (Show, Eq)

data HttpMethod = GET | POST deriving (Show, Eq)

data AuthType = TokenAuth deriving (Show, Eq)

data Header = Header String DataType deriving (Show, Eq)

data DataType = DataType String deriving (Show, Eq)
