{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SuspectFlagRequest where

import Data.Aeson
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Suspect
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SuspectFlagRequest = SuspectFlagRequest
  { adminApproval :: Domain.Types.SuspectFlagRequest.AdminApproval,
    approvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    flaggedBy :: Kernel.Prelude.Text,
    flaggedCategory :: Kernel.Prelude.Text,
    flaggedReason :: Kernel.Prelude.Text,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest,
    lastName :: Kernel.Prelude.Text,
    merchantShortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reportDetails :: Kernel.Prelude.Maybe Domain.Types.Suspect.ReportDetails,
    totalComplaintsCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AdminApproval = Pending | Approved | Rejected deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AdminApproval)

$(mkHttpInstancesForEnum ''AdminApproval)
