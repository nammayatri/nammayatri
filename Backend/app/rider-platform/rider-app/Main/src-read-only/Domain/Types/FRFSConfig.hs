{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Tools.Beam.UtilsTH

data FRFSConfig = FRFSConfig
  { bookingEndTime :: Kernel.Prelude.UTCTime,
    bookingStartTime :: Kernel.Prelude.UTCTime,
    busStationTtl :: Kernel.Types.Time.Seconds,
    cancellationReasonId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customDates :: [Kernel.Prelude.Text],
    customEndTime :: Kernel.Prelude.Text,
    discount :: Kernel.Prelude.Int,
    freeTicketInterval :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isCancellationAllowed :: Kernel.Prelude.Bool,
    isEventOngoing :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    maxFreeTicketCashback :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metroStationTtl :: Kernel.Prelude.Int,
    oneWayTicketLimit :: Kernel.Prelude.Int,
    providerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    radius :: Kernel.Types.Common.Meters,
    roundTripTicketLimit :: Kernel.Prelude.Int,
    straightLineDistance :: Kernel.Types.Common.Meters,
    validTillSeconds :: Kernel.Types.Time.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, Eq)
