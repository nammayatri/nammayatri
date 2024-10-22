{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Admin where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Suspect
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant hiding (Summary)
import "lib-dashboard" Tools.Auth

data AssignRoleMerchantUserReq = AssignRoleMerchantUserReq {email :: Data.Text.Text, roleName :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeleteMerchantUserReq = DeleteMerchantUserReq {email :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SuspectFlagChangeRequestList = SuspectFlagChangeRequestList {flaggedStatus :: Domain.Types.Suspect.FlaggedStatus, ids :: [Data.Text.Text], reasonToChange :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WebhookCheck = WebhookCheck {suspectList :: [WebhookRequest]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WebhookRequest = WebhookRequest {dl :: Data.Text.Text, flaggedBy :: Data.Text.Text, flaggedCategory :: Data.Text.Text, flaggedReason :: Data.Text.Text, voterId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
