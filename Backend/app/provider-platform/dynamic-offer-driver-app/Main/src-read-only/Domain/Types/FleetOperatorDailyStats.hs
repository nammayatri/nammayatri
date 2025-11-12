{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetOperatorDailyStats where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetOperatorDailyStats = FleetOperatorDailyStats
  { acceptationRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cashPlatformFees :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    customerCancellationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    distanceUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit,
    driverCancellationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    driverFirstSubscription :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    fleetOperatorId :: Kernel.Prelude.Text,
    inspectionCompleted :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantLocalDate :: Data.Time.Calendar.Day,
    onlineDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    onlinePlatformFees :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    pulledRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rejectedRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalCompletedRides :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    totalEarning :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalRatingCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalRatingScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
