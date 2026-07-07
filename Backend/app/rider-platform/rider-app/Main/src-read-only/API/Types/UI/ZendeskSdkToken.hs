{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ZendeskSdkToken where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data ZendeskJwtReq = ZendeskJwtReq {user_token :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ZendeskJwtResp = ZendeskJwtResp {jwt :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
