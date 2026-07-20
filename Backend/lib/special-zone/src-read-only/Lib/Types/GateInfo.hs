{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Types.GateInfo where

import qualified Data.Map.Strict
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data GateInfo = GateInfo
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    canQueueUpOnGate :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    defaultDemandThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultDriverExtra :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultMaxDriverThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultMinDriverThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    demandThresholds :: Kernel.Prelude.Maybe (Data.Map.Strict.Map Kernel.Prelude.Text Kernel.Prelude.Int),
    enableQueueFilter :: Kernel.Prelude.Maybe (Data.Map.Strict.Map Kernel.Prelude.Text Kernel.Prelude.Bool),
    entryFeeAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gateTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    gateType :: Lib.Types.GateInfo.GateType,
    geomGeoJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Types.GateInfo.GateInfo,
    maxDriverThresholds :: Kernel.Prelude.Maybe (Data.Map.Strict.Map Kernel.Prelude.Text Kernel.Prelude.Int),
    maxRideSkipsBeforeQueueRemoval :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.MerchantOperatingCity),
    minDriverThresholds :: Kernel.Prelude.Maybe (Data.Map.Strict.Map Kernel.Prelude.Text Kernel.Prelude.Int),
    name :: Kernel.Prelude.Text,
    navigationInstructions :: Kernel.Prelude.Maybe (Data.Map.Strict.Map Kernel.Prelude.Text Kernel.Prelude.Text),
    notificationActiveTillInSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    notificationCooldownInSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pickupRequestResponseTimeoutInSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pickupZoneArrivalTimeoutInSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    point :: Kernel.External.Maps.LatLong,
    specialLocationId :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation,
    updatedAt :: Kernel.Prelude.UTCTime,
    walkDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data GateType = Pickup | Drop | Parking deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''GateType))
