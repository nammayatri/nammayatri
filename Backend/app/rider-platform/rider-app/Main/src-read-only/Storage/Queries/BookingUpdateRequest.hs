{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingUpdateRequest where

import qualified Domain.Types.Booking
import qualified Domain.Types.BookingUpdateRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingUpdateRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BookingUpdateRequest.BookingUpdateRequest] -> m ())
createMany = traverse_ create

findAllByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m [Domain.Types.BookingUpdateRequest.BookingUpdateRequest])
findAllByBookingId bookingId = do findAllWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m (Maybe Domain.Types.BookingUpdateRequest.BookingUpdateRequest))
findById id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateErrorObjById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.BookingUpdateRequest.ErrorObj -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateErrorObjById errorObj id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.errorCode ((.errorCode) <$> errorObj),
      Se.Set Beam.errorMessage ((.errorMessage) <$> errorObj),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateMultipleById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateMultipleById travelledDistance estimatedDistance estimatedFare totalDistance currentPointLat currentPointLon id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.travelledDistance travelledDistance,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedFare estimatedFare,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.currentPointLat currentPointLat,
      Se.Set Beam.currentPointLon currentPointLon,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m (Maybe Domain.Types.BookingUpdateRequest.BookingUpdateRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateByPrimaryKey (Domain.Types.BookingUpdateRequest.BookingUpdateRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.status status,
      Se.Set Beam.travelledDistance travelledDistance,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedFare estimatedFare,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.currentPointLat currentPointLat,
      Se.Set Beam.currentPointLon currentPointLon,
      Se.Set Beam.oldEstimatedFare oldEstimatedFare,
      Se.Set Beam.oldEstimatedDistance oldEstimatedDistance,
      Se.Set Beam.errorCode ((.errorCode) <$> errorObj),
      Se.Set Beam.errorMessage ((.errorMessage) <$> errorObj),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BookingUpdateRequest Domain.Types.BookingUpdateRequest.BookingUpdateRequest where
  fromTType' (Beam.BookingUpdateRequestT {..}) = do
    pure $
      Just
        Domain.Types.BookingUpdateRequest.BookingUpdateRequest
          { id = Kernel.Types.Id.Id id,
            bookingId = Kernel.Types.Id.Id bookingId,
            status = status,
            travelledDistance = travelledDistance,
            estimatedDistance = estimatedDistance,
            estimatedFare = estimatedFare,
            totalDistance = totalDistance,
            currentPointLat = currentPointLat,
            currentPointLon = currentPointLon,
            oldEstimatedFare = oldEstimatedFare,
            oldEstimatedDistance = oldEstimatedDistance,
            errorObj = Domain.Types.BookingUpdateRequest.ErrorObj <$> errorCode <*> errorMessage,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            createdAt = createdAt,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId
          }

instance ToTType' Beam.BookingUpdateRequest Domain.Types.BookingUpdateRequest.BookingUpdateRequest where
  toTType' (Domain.Types.BookingUpdateRequest.BookingUpdateRequest {..}) = do
    Beam.BookingUpdateRequestT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.status = status,
        Beam.travelledDistance = travelledDistance,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedFare = estimatedFare,
        Beam.totalDistance = totalDistance,
        Beam.currentPointLat = currentPointLat,
        Beam.currentPointLon = currentPointLon,
        Beam.oldEstimatedFare = oldEstimatedFare,
        Beam.oldEstimatedDistance = oldEstimatedDistance,
        Beam.errorCode = (.errorCode) <$> errorObj,
        Beam.errorMessage = (.errorMessage) <$> errorObj,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId
      }
