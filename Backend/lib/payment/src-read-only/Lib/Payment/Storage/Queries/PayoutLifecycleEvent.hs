{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutLifecycleEvent (module Lib.Payment.Storage.Queries.PayoutLifecycleEvent, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutLifecycleEvent
import qualified Lib.Payment.Domain.Types.PayoutOrder
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutLifecycleEvent as Beam
import Lib.Payment.Storage.Queries.PayoutLifecycleEventExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent -> m ()
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => [Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent] -> m ()
createMany = traverse_ create

findById ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent ->
  m (Maybe Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPayoutOrderIdAndEventType ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutOrder.PayoutOrder ->
  (Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEventType -> m (Maybe Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent))
findByPayoutOrderIdAndEventType payoutOrderId eventType = do findOneWithKV [Se.And [Se.Is Beam.payoutOrderId $ Se.Eq (Kernel.Types.Id.getId payoutOrderId), Se.Is Beam.eventType $ Se.Eq eventType]]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent ->
  m (Maybe Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent -> m ()
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutLifecycleEvent.PayoutLifecycleEvent {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.batchId (Kernel.Types.Id.getId batchId),
      Se.Set Beam.batchNo batchNo,
      Se.Set Beam.currency currency,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.eventType eventType,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.partnerCode partnerCode,
      Se.Set Beam.payoutOrderId (Kernel.Types.Id.getId payoutOrderId),
      Se.Set Beam.payoutRequestId payoutRequestId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
