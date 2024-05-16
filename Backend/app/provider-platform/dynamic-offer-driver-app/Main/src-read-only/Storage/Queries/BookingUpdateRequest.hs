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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingUpdateRequest as Beam

create :: KvDbFlow m r => (Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.BookingUpdateRequest.BookingUpdateRequest] -> m ())
createMany = traverse_ create

findAllByBookingId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m [Domain.Types.BookingUpdateRequest.BookingUpdateRequest])
findAllByBookingId (Kernel.Types.Id.Id bookingId) = do findAllWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq bookingId]]

findByBAPBUReqId :: KvDbFlow m r => (Kernel.Prelude.Text -> m (Maybe Domain.Types.BookingUpdateRequest.BookingUpdateRequest))
findByBAPBUReqId bapBookingUpdateRequestId = do findOneWithKV [Se.And [Se.Is Beam.bapBookingUpdateRequestId $ Se.Eq bapBookingUpdateRequestId]]

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m (Maybe Domain.Types.BookingUpdateRequest.BookingUpdateRequest))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateMultipleById ::
  KvDbFlow m r =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateMultipleById travelledDistance estimatedFare totalDistance currentPointLat currentPointLon (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.travelledDistance travelledDistance,
      Se.Set Beam.estimatedFare estimatedFare,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.currentPointLat currentPointLat,
      Se.Set Beam.currentPointLon currentPointLon,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq id]

updateStatusByBAPBookingUpdateRequestId :: KvDbFlow m r => (Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus -> Kernel.Prelude.Text -> m ())
updateStatusByBAPBookingUpdateRequestId status bapBookingUpdateRequestId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.bapBookingUpdateRequestId $ Se.Eq bapBookingUpdateRequestId]

updateStatusById :: KvDbFlow m r => (Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateStatusById status (Kernel.Types.Id.Id id) = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

updateTravelledDistanceById :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateTravelledDistanceById travelledDistance (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.travelledDistance travelledDistance, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m (Maybe Domain.Types.BookingUpdateRequest.BookingUpdateRequest))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.BookingUpdateRequest.BookingUpdateRequest -> m ())
updateByPrimaryKey (Domain.Types.BookingUpdateRequest.BookingUpdateRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bapBookingUpdateRequestId bapBookingUpdateRequestId,
      Se.Set Beam.bookingId (Kernel.Types.Id.getId bookingId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currentPointLat currentPointLat,
      Se.Set Beam.currentPointLon currentPointLon,
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedFare estimatedFare,
      Se.Set Beam.fareParamsId (Kernel.Types.Id.getId fareParamsId),
      Se.Set Beam.farePolicyId (Kernel.Types.Id.getId farePolicyId),
      Se.Set Beam.maxEstimatedDistance maxEstimatedDistance,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.oldEstimatedDistance oldEstimatedDistance,
      Se.Set Beam.oldEstimatedFare oldEstimatedFare,
      Se.Set Beam.oldFareParamsId (Kernel.Types.Id.getId oldFareParamsId),
      Se.Set Beam.oldMaxEstimatedDistance oldMaxEstimatedDistance,
      Se.Set Beam.status status,
      Se.Set Beam.totalDistance totalDistance,
      Se.Set Beam.travelledDistance travelledDistance,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BookingUpdateRequest Domain.Types.BookingUpdateRequest.BookingUpdateRequest where
  fromTType' (Beam.BookingUpdateRequestT {..}) = do
    pure $
      Just
        Domain.Types.BookingUpdateRequest.BookingUpdateRequest
          { bapBookingUpdateRequestId = bapBookingUpdateRequestId,
            bookingId = Kernel.Types.Id.Id bookingId,
            createdAt = createdAt,
            currentPointLat = currentPointLat,
            currentPointLon = currentPointLon,
            estimatedDistance = estimatedDistance,
            estimatedFare = estimatedFare,
            fareParamsId = Kernel.Types.Id.Id fareParamsId,
            farePolicyId = Kernel.Types.Id.Id farePolicyId,
            id = Kernel.Types.Id.Id id,
            maxEstimatedDistance = maxEstimatedDistance,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            oldEstimatedDistance = oldEstimatedDistance,
            oldEstimatedFare = oldEstimatedFare,
            oldFareParamsId = Kernel.Types.Id.Id oldFareParamsId,
            oldMaxEstimatedDistance = oldMaxEstimatedDistance,
            status = status,
            totalDistance = totalDistance,
            travelledDistance = travelledDistance,
            updatedAt = updatedAt,
            validTill = validTill
          }

instance ToTType' Beam.BookingUpdateRequest Domain.Types.BookingUpdateRequest.BookingUpdateRequest where
  toTType' (Domain.Types.BookingUpdateRequest.BookingUpdateRequest {..}) = do
    Beam.BookingUpdateRequestT
      { Beam.bapBookingUpdateRequestId = bapBookingUpdateRequestId,
        Beam.bookingId = Kernel.Types.Id.getId bookingId,
        Beam.createdAt = createdAt,
        Beam.currentPointLat = currentPointLat,
        Beam.currentPointLon = currentPointLon,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedFare = estimatedFare,
        Beam.fareParamsId = Kernel.Types.Id.getId fareParamsId,
        Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxEstimatedDistance = maxEstimatedDistance,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.oldEstimatedDistance = oldEstimatedDistance,
        Beam.oldEstimatedFare = oldEstimatedFare,
        Beam.oldFareParamsId = Kernel.Types.Id.getId oldFareParamsId,
        Beam.oldMaxEstimatedDistance = oldMaxEstimatedDistance,
        Beam.status = status,
        Beam.totalDistance = totalDistance,
        Beam.travelledDistance = travelledDistance,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill
      }
