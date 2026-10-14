{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SupportBot where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data SupportBotConversationReq = SupportBotConversationReq
  { choice :: Kernel.Prelude.Maybe Data.Text.Text,
    context :: Kernel.Prelude.Maybe Data.Aeson.Value,
    conversationId :: Kernel.Prelude.Maybe Data.Text.Text,
    text :: Kernel.Prelude.Maybe Data.Text.Text,
    turnId :: Kernel.Prelude.Maybe Data.Text.Text,
    values :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
