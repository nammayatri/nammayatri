{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PenaltyRule where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PenaltyRule = PenaltyRule
  { id :: Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    triggerEvent :: PenaltyTriggerEvent,
    conditionsJson :: Kernel.Prelude.Text,
    penaltyType :: PenaltyAmountType,
    fixedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    percentage :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    formulaExpression :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    currency :: Kernel.Types.Common.Currency,
    gracePeriodCount :: Kernel.Prelude.Int,
    gracePeriodWindowHours :: Kernel.Prelude.Int,
    priority :: Kernel.Prelude.Int,
    isActive :: Kernel.Prelude.Bool,
    startDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    endDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq)

data PenaltyTriggerEvent
  = RIDE_CANCELLATION
  | SOS_FALSE_ALARM
  | DOCUMENT_EXPIRY
  | LOW_ACCEPTANCE_RATE
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

data PenaltyAmountType
  = FIXED
  | PERCENTAGE
  | FORMULA
  deriving (Read, Show, Eq, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Ord)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''PenaltyTriggerEvent)
$(Kernel.Utils.TH.mkHttpInstancesForEnum ''PenaltyAmountType)
$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PenaltyTriggerEvent)
$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PenaltyAmountType)
