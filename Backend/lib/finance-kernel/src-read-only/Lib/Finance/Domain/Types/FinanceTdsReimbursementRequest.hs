{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Core.Types
import qualified Tools.Beam.UtilsTH

data FinanceTdsReimbursementRequest = FinanceTdsReimbursementRequest
  { assessmentYear :: Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.AssessmentYear,
    certAmount :: Kernel.Types.Common.HighPrecMoney,
    certNumber :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    documentId :: Kernel.Types.Id.Id Lib.Finance.Core.Types.Image,
    fleetOwnerId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequest,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    quarter :: Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.Quarter,
    rejectionReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Finance.Domain.Types.FinanceTdsReimbursementRequest.FinanceTdsReimbursementRequestStatus,
    tanNumber :: Kernel.Prelude.Text,
    tdsRate :: Kernel.Prelude.Double,
    tdsSection :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

newtype AssessmentYear = AssessmentYear Kernel.Prelude.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FinanceTdsReimbursementRequestStatus = PENDING | APPROVED | REJECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Quarter = Q1 | Q2 | Q3 | Q4 deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AssessmentYear))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FinanceTdsReimbursementRequestStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''Quarter))
