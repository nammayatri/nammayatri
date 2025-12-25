{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Journey (module Storage.Queries.Journey, module ReExport) where

import qualified Domain.Types.Journey
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Sequelize as Se
import qualified Storage.Beam.Journey as Beam
import Storage.Queries.JourneyExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Journey.Journey] -> m ())
createMany = traverse_ create

findBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.Journey.Journey])
findBySearchId searchRequestId = do findAllWithKV [Se.Is Beam.searchRequestId $ Se.Eq searchRequestId]

updateHasStartedTrackingWithoutBooking :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updateHasStartedTrackingWithoutBooking hasStartedTrackingWithoutBooking id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.hasStartedTrackingWithoutBooking hasStartedTrackingWithoutBooking, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePaymentOrderShortId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updatePaymentOrderShortId paymentOrderShortId isPaymentSuccess id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.paymentOrderShortId (Kernel.Types.Id.getShortId <$> paymentOrderShortId),
      Se.Set Beam.isPaymentSuccess isPaymentSuccess,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.JourneyStatus -> Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status (Just status), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Journey.Journey -> m (Maybe Domain.Types.Journey.Journey))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Journey.Journey -> m ())
updateByPrimaryKey (Domain.Types.Journey.Journey {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.convenienceCost convenienceCost,
      Se.Set Beam.endTime endTime,
      Se.Set Beam.distanceUnit ((.unit) estimatedDistance),
      Se.Set Beam.estimatedDistance ((.value) estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.fromLocationAddress Nothing,
      Se.Set Beam.fromLocationId (Just $ Kernel.Types.Id.getId ((.id) fromLocation)),
      Se.Set Beam.hasPreferredServiceTier hasPreferredServiceTier,
      Se.Set Beam.hasPreferredTransitModes hasPreferredTransitModes,
      Se.Set Beam.hasStartedTrackingWithoutBooking hasStartedTrackingWithoutBooking,
      Se.Set Beam.isPaymentSuccess isPaymentSuccess,
      Se.Set Beam.isPublicTransportIncluded isPublicTransportIncluded,
      Se.Set Beam.isSingleMode isSingleMode,
      Se.Set Beam.journeyExpiryTime journeyExpiryTime,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.modes modes,
      Se.Set Beam.paymentOrderShortId (Kernel.Types.Id.getShortId <$> paymentOrderShortId),
      Se.Set Beam.recentLocationId (Kernel.Types.Id.getId <$> recentLocationId),
      Se.Set Beam.relevanceScore relevanceScore,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.searchRequestId searchRequestId,
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status (Just status),
      Se.Set Beam.toLocationAddress Nothing,
      Se.Set Beam.toLocationId (Kernel.Types.Id.getId <$> (toLocation <&> (.id))),
      Se.Set Beam.totalLegs totalLegs,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
