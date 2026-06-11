{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SpecialZoneQueueRequestExtra where

import qualified Domain.Types.Person
import qualified Domain.Types.SpecialZoneQueueRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialZoneQueueRequest as Beam
import Storage.Queries.OrphanInstances.SpecialZoneQueueRequest

-- Extra code goes here --
findActiveByStasusListAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus] ->
  m [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest]
findActiveByStasusListAndDriverId driverId statusList =
  findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.In statusList]]

findAllByDriverIdsAndStatuses ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Kernel.Types.Id.Id Domain.Types.Person.Person] ->
  [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus] ->
  m [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest]
findAllByDriverIdsAndStatuses driverIds statuses
  | null driverIds || null statuses = pure []
  | otherwise =
    findAllWithKV
      [ Se.And
          [ Se.Is Beam.driverId $ Se.In (Kernel.Types.Id.getId <$> driverIds),
            Se.Is Beam.status $ Se.In statuses
          ]
      ]

-- | Finalize a trigger batch: mark every still-Active (un-responded) row for the
-- given triggerRequestId as Ignored/Expired. Called once per trigger by the status
-- endpoint's cleanup pass after the retry loop has stopped, so rows whose driver
-- never opened the app still settle to a terminal state for audit. Bulk single-shot
-- update — no fetch, no per-row round-trips.
markRemainingIgnoredByTriggerRequestId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  m ()
markRemainingIgnoredByTriggerRequestId triggerRequestId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.response (Just Domain.Types.SpecialZoneQueueRequest.Ignored),
      Se.Set Beam.status Domain.Types.SpecialZoneQueueRequest.Expired,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.triggerRequestId $ Se.Eq (Just triggerRequestId),
          Se.Is Beam.status $ Se.Eq Domain.Types.SpecialZoneQueueRequest.Active
        ]
    ]

-- | Transition a pickup-zone request to Accepted and stamp the arrival deadline.
-- The arrival deadline lives on its own column (arrivalDeadlineTime) rather than
-- repurposing validTill — that way validTill keeps its original "Active window
-- expiry" meaning, and the GET handler can detect a missed arrival cleanly via
-- arrivalDeadlineTime < now.
updateToAcceptedWithArrivalDeadline ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest ->
  Kernel.Prelude.UTCTime ->
  m ()
updateToAcceptedWithArrivalDeadline requestId arrivalDeadline = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.response (Just Domain.Types.SpecialZoneQueueRequest.Accept),
      Se.Set Beam.status Domain.Types.SpecialZoneQueueRequest.Accepted,
      Se.Set Beam.arrivalDeadlineTime (Just arrivalDeadline),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId requestId)]

-- | Find pickup-zone requests at a given gate filtered by status and vehicle type,
-- restricted to createdAt >= the passed-in lower bound. Uses conditional-DB fallback
-- (no KV) because gateId / status / vehicleType are not indexed; the createdAt bound
-- keeps the scan cheap on the raw table. The caller owns the time cutoff so repeated
-- calls (e.g. per-variant in a loop) share a single getCurrentTime.
findAllByGateIdStatusAndVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Text ->
  Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus ->
  Kernel.Prelude.Text ->
  m [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest]
findAllByGateIdStatusAndVehicleType createdAtFrom gateId status vehicleType =
  findAllWithKVAndConditionalDB
    [ Se.And
        [ Se.Is Beam.createdAt $ Se.GreaterThanOrEq createdAtFrom,
          Se.Is Beam.gateId $ Se.Eq gateId,
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]
    Nothing

-- | Find the most recent pickup-zone request for a driver where response=Accept,
-- regardless of its current status. This catches requests that have already
-- transitioned past Accepted (e.g., Expired after the arrival check) so the
-- supply decrement still runs in the normal Confirm / StartRide flows.
findLastAcceptedByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m (Maybe Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest)
findLastAcceptedByDriverId driverId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.response $ Se.Eq (Just Domain.Types.SpecialZoneQueueRequest.Accept)
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe
