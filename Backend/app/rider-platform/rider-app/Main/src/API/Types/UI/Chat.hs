{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Chat where

import Data.OpenApi (ToSchema)
import Kernel.Prelude
import Kernel.Utils.TH

data ChatReq = ChatReq
  { messages :: [ChatMessage]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChatRes = ChatRes
  { message :: ChatMessage,
    toolCalls :: Maybe [ToolCall]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChatMessage = ChatMessage
  { role :: ChatRole,
    content :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChatRole
  = User
  | Assistant
  | Tool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

data ToolCall = ToolCall
  { name :: Text,
    arguments :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''ChatRole)
