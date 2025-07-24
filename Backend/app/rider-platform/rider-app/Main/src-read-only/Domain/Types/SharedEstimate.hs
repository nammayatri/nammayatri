{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedEstimate where

import Data.Aeson
import qualified Data.Time.LocalTime
import qualified Domain.Types.Common
import qualified Domain.Types.Estimate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedSearchRequest
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedEstimate = SharedEstimate
  { bppSharedEstimateId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    estimateIds :: [Kernel.Types.Id.Id Domain.Types.Estimate.Estimate],
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedTotalFare :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    nightShiftCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    nightShiftChargeAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    nightShiftEnd :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    nightShiftStart :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    oldNightShiftCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    providerId :: Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Types.Common.BaseUrl,
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sharedSearchRequestId :: Kernel.Types.Id.Id Domain.Types.SharedSearchRequest.SharedSearchRequest,
    status :: Domain.Types.SharedEstimate.EstimateStatus,
    totalFareRangeMax :: Kernel.Types.Common.HighPrecMoney,
    totalFareRangeMin :: Kernel.Types.Common.HighPrecMoney,
    tripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleServiceTierSeatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, Show)

data EstimateStatus
  = NEW
  | DRIVER_QUOTE_REQUESTED
  | CANCELLED
  | GOT_DRIVER_QUOTE
  | DRIVER_QUOTE_CANCELLED
  | COMPLETED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EstimateStatus))

$(mkHttpInstancesForEnum (''EstimateStatus))
