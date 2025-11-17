{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetRcAssociationDailyStats where

import Data.Aeson
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetRcAssociationDailyStats = FleetRcAssociationDailyStats
  { currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit,
    fleetOwnerId :: Data.Text.Text,
    merchantLocalDate :: Data.Time.Calendar.Day,
    rcId :: Data.Text.Text,
    rideDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    rideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    totalCompletedRides :: Kernel.Prelude.Int,
    totalEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
