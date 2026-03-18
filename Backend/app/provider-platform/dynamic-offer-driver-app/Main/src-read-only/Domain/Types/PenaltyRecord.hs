{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PenaltyRecord where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PenaltyRule
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PenaltyRecord = PenaltyRecord
  { id :: Kernel.Types.Id.Id Domain.Types.PenaltyRecord.PenaltyRecord,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    ruleId :: Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    triggerEvent :: Domain.Types.PenaltyRule.PenaltyTriggerEvent,
    triggerEntityId :: Kernel.Prelude.Text,
    amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    reason :: Kernel.Prelude.Text,
    status :: PenaltyStatus,
    disputeReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disputeEvidence :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disputeResolvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disputeResolvedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    ledgerEntryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq)

data PenaltyStatus
  = PENDING
  | APPLIED
  | DISPUTED
  | WAIVED
  | REJECTED
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''PenaltyStatus)
$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PenaltyStatus)
