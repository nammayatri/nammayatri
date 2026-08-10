{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSBookingRating where

import qualified Data.Text
import qualified Domain.Types.FRFSBookingRating
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSBookingRating as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingRating.FRFSBookingRating -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSBookingRating.FRFSBookingRating] -> m ())
createMany = traverse_ create

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FRFSBookingRating.FRFSBookingRating]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.FRFSBookingRating.FRFSBookingRating))
findByBookingId bookingId = do findOneWithKV [Se.Is Beam.bookingId $ Se.Eq bookingId]

updateRating ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.FRFSBookingRating.FRFSBookingRating -> m ())
updateRating driverRatingValue fleetRatingValue feedbackDetails id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.driverRatingValue driverRatingValue,
      Se.Set Beam.fleetRatingValue fleetRatingValue,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSBookingRating.FRFSBookingRating -> m (Maybe Domain.Types.FRFSBookingRating.FRFSBookingRating))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSBookingRating.FRFSBookingRating -> m ())
updateByPrimaryKey (Domain.Types.FRFSBookingRating.FRFSBookingRating {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingId bookingId,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.driverRatingValue driverRatingValue,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.fleetNumber fleetNumber,
      Se.Set Beam.fleetRatingValue fleetRatingValue,
      Se.Set Beam.gtfsId gtfsId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.operatorBadgeToken operatorBadgeToken,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSBookingRating Domain.Types.FRFSBookingRating.FRFSBookingRating where
  fromTType' (Beam.FRFSBookingRatingT {..}) = do
    pure $
      Just
        Domain.Types.FRFSBookingRating.FRFSBookingRating
          { bookingId = bookingId,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            driverRatingValue = driverRatingValue,
            feedbackDetails = feedbackDetails,
            fleetNumber = fleetNumber,
            fleetRatingValue = fleetRatingValue,
            gtfsId = gtfsId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            operatorBadgeToken = operatorBadgeToken,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSBookingRating Domain.Types.FRFSBookingRating.FRFSBookingRating where
  toTType' (Domain.Types.FRFSBookingRating.FRFSBookingRating {..}) = do
    Beam.FRFSBookingRatingT
      { Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverRatingValue = driverRatingValue,
        Beam.feedbackDetails = feedbackDetails,
        Beam.fleetNumber = fleetNumber,
        Beam.fleetRatingValue = fleetRatingValue,
        Beam.gtfsId = gtfsId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.operatorBadgeToken = operatorBadgeToken,
        Beam.updatedAt = updatedAt
      }
