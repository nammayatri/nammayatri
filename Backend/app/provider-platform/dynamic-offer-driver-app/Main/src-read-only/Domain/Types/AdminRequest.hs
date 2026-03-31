{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AdminRequest where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data AdminRequest = AdminRequest
  { actionType :: Domain.Types.AdminRequest.ActionType,
    adjustmentType :: Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType,
    adminCheckerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    adminCheckerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminMakerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    adminMakerName :: Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    referenceId :: Kernel.Prelude.Text,
    referenceTable :: Domain.Types.AdminRequest.ReferenceTable,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource,
    status :: Domain.Types.AdminRequest.AdminRequestStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ActionType = LedgerAdjustment | FailedPayoutReTrigger | TDSReimbursement deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AdjustmentSource = BuyerApp | Internal deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AdjustmentType = Debit | Credit deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AdminRequestStatus = PENDING | APPROVED | REJECTED | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReferenceTable = BOOKING | RIDE | PAYOUT_REQUEST deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ActionType)

$(mkHttpInstancesForEnum ''ActionType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AdjustmentSource)

$(mkHttpInstancesForEnum ''AdjustmentSource)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AdjustmentType)

$(mkHttpInstancesForEnum ''AdjustmentType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AdminRequestStatus)

$(mkHttpInstancesForEnum ''AdminRequestStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ReferenceTable)

$(mkHttpInstancesForEnum ''ReferenceTable)
