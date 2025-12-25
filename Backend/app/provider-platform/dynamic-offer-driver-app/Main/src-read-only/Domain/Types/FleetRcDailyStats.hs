{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetRcDailyStats where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetRcDailyStats = FleetRcDailyStats
  { currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    fleetOwnerId :: Kernel.Prelude.Text,
    merchantLocalDate :: Data.Time.Calendar.Day,
    rcId :: Kernel.Prelude.Text,
    rideDistance :: Kernel.Types.Common.Meters,
    rideDuration :: Kernel.Types.Common.Seconds,
    totalCompletedRides :: Kernel.Prelude.Int,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
