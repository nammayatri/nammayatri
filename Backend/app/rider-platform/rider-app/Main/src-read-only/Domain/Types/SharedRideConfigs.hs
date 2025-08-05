{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedRideConfigs where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SharedRideConfigs = SharedRideConfigs
  { actualDropDistanceThreshold :: Kernel.Types.Common.Meters,
    actualPickupDistanceThreshold :: Kernel.Types.Common.Meters,
    createdAt :: Kernel.Prelude.UTCTime,
    customerRemainingThresholdForFlowContinuation :: Kernel.Prelude.Int,
    dropLocationSearchRadius :: Kernel.Types.Common.Meters,
    enableSharedRide :: Kernel.Prelude.Bool,
    enableSyncPooling :: Kernel.Prelude.Bool,
    geoHashPrecisionForRouteMatching :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.SharedRideConfigs.SharedRideConfigs,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pickupLocationSearchRadius :: Kernel.Types.Common.Meters,
    routeMatchingThreshold :: Kernel.Prelude.Double,
    routeOverlapThreshold :: Kernel.Prelude.Double,
    searchExpiryBufferSeconds :: Kernel.Types.Common.Seconds,
    searchRequestExpirySeconds :: Kernel.Types.Common.Seconds,
    searchThresholdForSharedEstimate :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory
  }
  deriving (Generic, Show)
