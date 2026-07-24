{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ZendeskWebhook where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data ZendeskWebhookPayload = ZendeskWebhookPayload {status :: Kernel.Prelude.Maybe Kernel.Prelude.Text, ticket_id :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
