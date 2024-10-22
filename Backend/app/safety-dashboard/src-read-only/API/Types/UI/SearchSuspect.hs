{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SearchSuspect where

import qualified API.Types.UI.Notification
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant hiding (Summary)
import "lib-dashboard" Tools.Auth

data CheckSuspectStatusHistoryResp = CheckSuspectStatusHistoryResp {suspectStatusHistory :: [StatusHistory]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchSuspectReq = SearchSuspectReq {dl :: Kernel.Prelude.Maybe Data.Text.Text, voterId :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchSuspectReqList = SearchSuspectReqList {suspectReqList :: [SearchSuspectReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchSuspectResp = SearchSuspectResp
  { createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    flagUpdatedAt :: Kernel.Prelude.UTCTime,
    flaggedBy :: Kernel.Prelude.Maybe [Domain.Types.Suspect.FlaggedBy],
    flaggedCounter :: Kernel.Prelude.Int,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Data.Text.Text,
    lastName :: Data.Text.Text,
    statusChangedReason :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StatusHistory = StatusHistory
  { adminApproval :: Kernel.Prelude.Maybe Domain.Types.SuspectFlagRequest.AdminApproval,
    approvedBy :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Kernel.Prelude.Maybe Data.Text.Text,
    flaggedBy :: Kernel.Prelude.Maybe [Domain.Types.Suspect.FlaggedBy],
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Data.Text.Text,
    lastName :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantShortId :: Kernel.Prelude.Maybe Data.Text.Text,
    statusChangedReason :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SuspectsList = SuspectsList {summary :: API.Types.UI.Notification.Summary, suspects :: [SearchSuspectResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
