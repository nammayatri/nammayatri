{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SuspectFlagRequest where

import qualified API.Types.UI.Notification
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant hiding (Summary)
import "lib-dashboard" Tools.Auth

data SuspectApprovalReqList = SuspectApprovalReqList {adminApproval :: Domain.Types.SuspectFlagRequest.AdminApproval, suspectFlagRequestIdList :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SuspectFlagRequestList = SuspectFlagRequestList {flagRequestList :: [SuspectFlagRequestResp], summary :: API.Types.UI.Notification.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SuspectFlagRequestResp = SuspectFlagRequestResp
  { adminApproval :: Domain.Types.SuspectFlagRequest.AdminApproval,
    createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    flaggedBy :: Data.Text.Text,
    flaggedCategory :: Data.Text.Text,
    flaggedReason :: Data.Text.Text,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Data.Text.Text,
    lastName :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SuspectsApprovalReq = SuspectsApprovalReq {adminApproval :: Domain.Types.SuspectFlagRequest.AdminApproval, suspectFlagRequestId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
