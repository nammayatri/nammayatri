{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Queries.OrphanInstances.GateInfo where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Queries.Transformers.GateInfo
import qualified Lib.Storage.Beam.GateInfo as Beam
import qualified Lib.Types.GateInfo

instance FromTType' Beam.GateInfo Lib.Types.GateInfo.GateInfo where
  fromTType' (Beam.GateInfoT {..}) = do
    pure $
      Just
        Lib.Types.GateInfo.GateInfo
          { address = address,
            canQueueUpOnGate = canQueueUpOnGate,
            createdAt = createdAt,
            defaultDemandThreshold = defaultDemandThreshold,
            defaultDriverExtra = defaultDriverExtra,
            defaultMaxDriverThreshold = defaultMaxDriverThreshold,
            defaultMinDriverThreshold = defaultMinDriverThreshold,
            demandThresholds = Lib.Queries.Transformers.GateInfo.decodeThresholdMap demandThresholdsJson,
            enableQueueFilter = Lib.Queries.Transformers.GateInfo.decodeBoolMap enableQueueFilter,
            entryFeeAmount = entryFeeAmount,
            gateTags = gateTags,
            gateType = gateType,
            geomGeoJson = geomGeoJson,
            id = Kernel.Types.Id.Id id,
            maxDriverThresholds = Lib.Queries.Transformers.GateInfo.decodeThresholdMap maxDriverThresholdsJson,
            maxRideSkipsBeforeQueueRemoval = maxRideSkipsBeforeQueueRemoval,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            minDriverThresholds = Lib.Queries.Transformers.GateInfo.decodeThresholdMap minDriverThresholdsJson,
            name = name,
            navigationInstructions = Lib.Queries.Transformers.GateInfo.decodeTextMap navigationInstructionsJson,
            notificationActiveTillInSec = notificationActiveTillInSec,
            notificationCooldownInSec = notificationCooldownInSec,
            pickupRequestResponseTimeoutInSec = pickupRequestResponseTimeoutInSec,
            pickupZoneArrivalTimeoutInSec = pickupZoneArrivalTimeoutInSec,
            point = Lib.Queries.Transformers.GateInfo.mkLatLongFromText point,
            specialLocationId = Kernel.Types.Id.Id specialLocationId,
            updatedAt = updatedAt,
            walkDescription = walkDescription
          }

instance ToTType' Beam.GateInfo Lib.Types.GateInfo.GateInfo where
  toTType' (Lib.Types.GateInfo.GateInfo {..}) = do
    Beam.GateInfoT
      { Beam.address = address,
        Beam.canQueueUpOnGate = canQueueUpOnGate,
        Beam.createdAt = createdAt,
        Beam.defaultDemandThreshold = defaultDemandThreshold,
        Beam.defaultDriverExtra = defaultDriverExtra,
        Beam.defaultMaxDriverThreshold = defaultMaxDriverThreshold,
        Beam.defaultMinDriverThreshold = defaultMinDriverThreshold,
        Beam.demandThresholdsJson = Lib.Queries.Transformers.GateInfo.encodeThresholdMap demandThresholds,
        Beam.enableQueueFilter = Lib.Queries.Transformers.GateInfo.encodeBoolMap enableQueueFilter,
        Beam.entryFeeAmount = entryFeeAmount,
        Beam.gateTags = gateTags,
        Beam.gateType = gateType,
        Beam.geomGeoJson = geomGeoJson,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxDriverThresholdsJson = Lib.Queries.Transformers.GateInfo.encodeThresholdMap maxDriverThresholds,
        Beam.maxRideSkipsBeforeQueueRemoval = maxRideSkipsBeforeQueueRemoval,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.minDriverThresholdsJson = Lib.Queries.Transformers.GateInfo.encodeThresholdMap minDriverThresholds,
        Beam.name = name,
        Beam.navigationInstructionsJson = Lib.Queries.Transformers.GateInfo.encodeTextMap navigationInstructions,
        Beam.notificationActiveTillInSec = notificationActiveTillInSec,
        Beam.notificationCooldownInSec = notificationCooldownInSec,
        Beam.pickupRequestResponseTimeoutInSec = pickupRequestResponseTimeoutInSec,
        Beam.pickupZoneArrivalTimeoutInSec = pickupZoneArrivalTimeoutInSec,
        Beam.point = Lib.Queries.Transformers.GateInfo.latLongToText point,
        Beam.specialLocationId = Kernel.Types.Id.getId specialLocationId,
        Beam.updatedAt = updatedAt,
        Beam.walkDescription = walkDescription
      }
