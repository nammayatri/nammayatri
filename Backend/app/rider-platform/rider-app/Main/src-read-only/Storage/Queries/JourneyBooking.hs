{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.JourneyBooking where

import qualified Domain.Types.JourneyBooking
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.JourneyBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyBooking.JourneyBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.JourneyBooking.JourneyBooking] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.JourneyBooking.JourneyBooking -> m (Maybe Domain.Types.JourneyBooking.JourneyBooking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.JourneyBooking.JourneyBooking -> m ())
updateByPrimaryKey (Domain.Types.JourneyBooking.JourneyBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.convenienceCost convenienceCost,
      Se.Set Beam.customerCancelled customerCancelled,
      Se.Set Beam.distanceUnit ((.unit) estimatedDistance),
      Se.Set Beam.estimatedDistance ((.value) estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.estimatedFare (Kernel.Prelude.fmap (.amount) estimatedFare),
      Se.Set Beam.currency (Kernel.Prelude.fmap (.currency) fare),
      Se.Set Beam.fare (Kernel.Prelude.fmap (.amount) fare),
      Se.Set Beam.isBookingCancellable isBookingCancellable,
      Se.Set Beam.journeyId (Kernel.Types.Id.getId journeyId),
      Se.Set Beam.modes modes,
      Se.Set Beam.numberOfPassengers numberOfPassengers,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.JourneyBooking Domain.Types.JourneyBooking.JourneyBooking where
  fromTType' (Beam.JourneyBookingT {..}) = do
    pure $
      Just
        Domain.Types.JourneyBooking.JourneyBooking
          { convenienceCost = convenienceCost,
            customerCancelled = customerCancelled,
            estimatedDistance = Kernel.Types.Common.Distance estimatedDistance distanceUnit,
            estimatedDuration = estimatedDuration,
            estimatedFare = Kernel.Types.Common.mkPrice currency <$> estimatedFare,
            fare = Kernel.Types.Common.mkPrice currency <$> fare,
            id = Kernel.Types.Id.Id id,
            isBookingCancellable = isBookingCancellable,
            journeyId = Kernel.Types.Id.Id journeyId,
            modes = modes,
            numberOfPassengers = numberOfPassengers,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JourneyBooking Domain.Types.JourneyBooking.JourneyBooking where
  toTType' (Domain.Types.JourneyBooking.JourneyBooking {..}) = do
    Beam.JourneyBookingT
      { Beam.convenienceCost = convenienceCost,
        Beam.customerCancelled = customerCancelled,
        Beam.distanceUnit = (.unit) estimatedDistance,
        Beam.estimatedDistance = (.value) estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedFare = Kernel.Prelude.fmap (.amount) estimatedFare,
        Beam.currency = Kernel.Prelude.fmap (.currency) fare,
        Beam.fare = Kernel.Prelude.fmap (.amount) fare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBookingCancellable = isBookingCancellable,
        Beam.journeyId = Kernel.Types.Id.getId journeyId,
        Beam.modes = modes,
        Beam.numberOfPassengers = numberOfPassengers,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
