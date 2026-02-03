{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DailyStats where

import Data.Aeson
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DailyStats = DailyStats
  { activatedValidRides :: Kernel.Prelude.Int,
    bonusEarnings :: Kernel.Types.Common.HighPrecMoney,
    cancellationCharges :: Kernel.Types.Common.HighPrecMoney,
    commissionCharges :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Data.Text.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantLocalDate :: Data.Time.Calendar.Day,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    numDriversOnboarded :: Kernel.Prelude.Int,
    numFleetsOnboarded :: Kernel.Prelude.Int,
    numRides :: Kernel.Prelude.Int,
    onlineDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    payoutOrderId :: Kernel.Prelude.Maybe Data.Text.Text,
    payoutOrderStatus :: Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus,
    payoutStatus :: Domain.Types.DailyStats.PayoutStatus,
    referralCounts :: Kernel.Prelude.Int,
    referralEarnings :: Kernel.Types.Common.HighPrecMoney,
    tipAmount :: Kernel.Types.Common.HighPrecMoney,
    tollCharges :: Kernel.Types.Common.HighPrecMoney,
    totalDistance :: Kernel.Types.Common.Meters,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    totalRideTime :: Kernel.Types.Common.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PayoutStatus = Verifying | Processing | Success | Failed | ManualReview | PendingForVpa | Initialized deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutStatus)
