{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ScheduledPayout where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data ScheduledPayout = ScheduledPayout
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bookingId :: Kernel.Prelude.Text,
    cancelReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideId :: Kernel.Prelude.Text,
    status :: Domain.Types.ScheduledPayout.ScheduledPayoutStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data ScheduledPayoutStatus = PENDING | PROCESSING | PROCESSED | FAILED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ScheduledPayoutStatus))
