{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters
import qualified Tools.Beam.UtilsTH

data MerchantConfig = MerchantConfig
  { createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    enabled :: Kernel.Prelude.Bool,
    fraudBookingCancellationCountThreshold :: Kernel.Prelude.Int,
    fraudBookingCancellationCountWindow :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudBookingCancelledByDriverCountThreshold :: Kernel.Prelude.Int,
    fraudBookingCancelledByDriverCountWindow :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudBookingTotalCountThreshold :: Kernel.Prelude.Int,
    fraudRideCountThreshold :: Kernel.Prelude.Int,
    fraudRideCountWindow :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudSearchCountThreshold :: Kernel.Prelude.Int,
    fraudSearchCountWindow :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    id :: Kernel.Types.Id.Id Domain.Types.MerchantConfig.MerchantConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)
