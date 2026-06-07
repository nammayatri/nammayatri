{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Queries.GateInfo (module Lib.Queries.GateInfo, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Lib.Queries.GateInfoExtra as ReExport
import qualified Lib.Queries.Transformers.GateInfo
import qualified Lib.Storage.Beam.BeamFlow
import qualified Lib.Storage.Beam.GateInfo as Beam
import qualified Lib.Types.GateInfo
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se

create :: (Lib.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Types.GateInfo.GateInfo -> m ())
create = createWithKV

createMany :: (Lib.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Types.GateInfo.GateInfo] -> m ())
createMany = traverse_ create

findAllBySpecialLocationId :: (Lib.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> m ([Lib.Types.GateInfo.GateInfo]))
findAllBySpecialLocationId specialLocationId = do findAllWithKV [Se.Is Beam.specialLocationId $ Se.Eq (Kernel.Types.Id.getId specialLocationId)]

findById :: (Lib.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Types.GateInfo.GateInfo -> m (Maybe Lib.Types.GateInfo.GateInfo))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Lib.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Types.GateInfo.GateInfo -> m (Maybe Lib.Types.GateInfo.GateInfo))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Types.GateInfo.GateInfo -> m ())
updateByPrimaryKey (Lib.Types.GateInfo.GateInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.canQueueUpOnGate canQueueUpOnGate,
      Se.Set Beam.defaultDemandThreshold defaultDemandThreshold,
      Se.Set Beam.defaultDriverExtra defaultDriverExtra,
      Se.Set Beam.defaultMaxDriverThreshold defaultMaxDriverThreshold,
      Se.Set Beam.defaultMinDriverThreshold defaultMinDriverThreshold,
      Se.Set Beam.demandThresholdsJson (Lib.Queries.Transformers.GateInfo.encodeThresholdMap demandThresholds),
      Se.Set Beam.enableQueueFilter (Lib.Queries.Transformers.GateInfo.encodeBoolMap enableQueueFilter),
      Se.Set Beam.entryFeeAmount entryFeeAmount,
      Se.Set Beam.gateTags gateTags,
      Se.Set Beam.gateType gateType,
      Se.Set Beam.geomGeoJson geomGeoJson,
      Se.Set Beam.maxDriverThresholdsJson (Lib.Queries.Transformers.GateInfo.encodeThresholdMap maxDriverThresholds),
      Se.Set Beam.maxRideSkipsBeforeQueueRemoval maxRideSkipsBeforeQueueRemoval,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.minDriverThresholdsJson (Lib.Queries.Transformers.GateInfo.encodeThresholdMap minDriverThresholds),
      Se.Set Beam.name name,
      Se.Set Beam.notificationActiveTillInSec notificationActiveTillInSec,
      Se.Set Beam.notificationCooldownInSec notificationCooldownInSec,
      Se.Set Beam.pickupRequestResponseTimeoutInSec pickupRequestResponseTimeoutInSec,
      Se.Set Beam.pickupZoneArrivalTimeoutInSec pickupZoneArrivalTimeoutInSec,
      Se.Set Beam.point (Lib.Queries.Transformers.GateInfo.latLongToText point),
      Se.Set Beam.specialLocationId (Kernel.Types.Id.getId specialLocationId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.walkDescription walkDescription
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
