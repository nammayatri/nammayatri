{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedEstimate where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.Common
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedEstimate = SharedEstimate
  { createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Utils.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    id :: Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    status :: Domain.Types.SharedEstimate.EstimateStatus,
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    totalMaxFare :: Kernel.Types.Common.HighPrecMoney,
    totalMinFare :: Kernel.Types.Common.HighPrecMoney,
    transactionId :: Kernel.Prelude.Text,
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.ServiceTierType.ServiceTierType
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
