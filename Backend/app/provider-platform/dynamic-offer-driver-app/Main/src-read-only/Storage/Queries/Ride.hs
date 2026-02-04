{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Ride (module Storage.Queries.Ride, module ReExport) where

import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Tools.Utils
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.Ride as Beam
import Storage.Queries.RideExtra as ReExport
import Storage.Queries.Transformers.Ride

updateCancellationChargesOnCancel :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateCancellationChargesOnCancel cancellationChargesOnCancel id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cancellationChargesOnCancel cancellationChargesOnCancel, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCancellationFeeIfCancelledField :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateCancellationFeeIfCancelledField cancellationFeeIfCancelled id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.cancellationFeeIfCancelled cancellationFeeIfCancelled, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDriverCancellationDeductionOnNextRide ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateDriverCancellationDeductionOnNextRide driverCancellationDeductionOnNextRideAmount id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverCancellationDeductionOnNextRideAmount driverCancellationDeductionOnNextRideAmount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDriverCancellationPenalty ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateDriverCancellationPenalty driverCancellationPenaltyFeeId driverCancellationPenaltyAmount id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.driverCancellationPenaltyFeeId driverCancellationPenaltyFeeId,
      Se.Set Beam.driverCancellationPenaltyAmount driverCancellationPenaltyAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDriverCancellationPenaltyWaivedReasonAndAmount ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateDriverCancellationPenaltyWaivedReasonAndAmount driverCancellationPenaltyWaivedReason driverCancellationPenaltyAmount id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.driverCancellationPenaltyWaivedReason driverCancellationPenaltyWaivedReason,
      Se.Set Beam.driverCancellationPenaltyAmount driverCancellationPenaltyAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDriverGpsTurnedOff :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateDriverGpsTurnedOff driverGpsTurnedOff id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverGpsTurnedOff driverGpsTurnedOff, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateEstimatedEndTimeRange :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Domain.Types.Ride.EstimatedEndTimeRange -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateEstimatedEndTimeRange estimatedEndTimeRange id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.estimatedEndTimeRangeEnd (Kernel.Prelude.fmap (.end) estimatedEndTimeRange),
      Se.Set Beam.estimatedEndTimeRangeStart (Kernel.Prelude.fmap (.start) estimatedEndTimeRange),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFinalFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateFinalFarePolicyId finalFarePolicyId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.finalFarePolicyId (Kernel.Types.Id.getId <$> finalFarePolicyId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsPickupOrDestinationEdited :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateIsPickupOrDestinationEdited isPickupOrDestinationEdited id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isPickupOrDestinationEdited isPickupOrDestinationEdited, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePreviousRideTripEndPosAndTime ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.External.Maps.LatLong -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updatePreviousRideTripEndPosAndTime previousRideTripEndPos previousRideTripEndTime id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.previousRideTripEndLat (Kernel.Prelude.fmap (.lat) previousRideTripEndPos),
      Se.Set Beam.previousRideTripEndLon (Kernel.Prelude.fmap (.lon) previousRideTripEndPos),
      Se.Set Beam.previousRideTripEndTime previousRideTripEndTime,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRideTags :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValue] -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateRideTags rideTags id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.rideTags (Lib.Yudhishthira.Tools.Utils.tagsNameValueToTType rideTags), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTipAmountField :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m ())
updateTipAmountField tipAmount id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.tipAmount tipAmount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
