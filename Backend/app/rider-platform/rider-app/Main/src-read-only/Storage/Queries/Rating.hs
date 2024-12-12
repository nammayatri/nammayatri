{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Rating where

import qualified Domain.Types.Person
import qualified Domain.Types.Rating
import qualified Domain.Types.Ride
import qualified IssueManagement.Domain.Types.MediaFile
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Rating as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Rating.Rating -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Rating.Rating] -> m ())
createMany = traverse_ create

findAllRatingsForPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.Rating.Rating])
findAllRatingsForPerson riderId = do findAllWithDb [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findRatingForRide :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m (Maybe Domain.Types.Rating.Rating))
findRatingForRide rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)]

updateRating ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile) -> Kernel.Types.Id.Id Domain.Types.Rating.Rating -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateRating ratingValue feedbackDetails wasOfferedAssistance mediaId id riderId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.ratingValue ratingValue,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.wasOfferedAssistance wasOfferedAssistance,
      Se.Set Beam.mediaId (Kernel.Types.Id.getId <$> mediaId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Rating.Rating -> m (Maybe Domain.Types.Rating.Rating))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Rating.Rating -> m ())
updateByPrimaryKey (Domain.Types.Rating.Rating {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.feedbackDetails feedbackDetails,
      Se.Set Beam.mediaId (Kernel.Types.Id.getId <$> mediaId),
      Se.Set Beam.ratingValue ratingValue,
      Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.wasOfferedAssistance wasOfferedAssistance,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Rating Domain.Types.Rating.Rating where
  fromTType' (Beam.RatingT {..}) = do
    pure $
      Just
        Domain.Types.Rating.Rating
          { createdAt = createdAt,
            feedbackDetails = feedbackDetails,
            id = Kernel.Types.Id.Id id,
            mediaId = Kernel.Types.Id.Id <$> mediaId,
            ratingValue = ratingValue,
            rideId = Kernel.Types.Id.Id rideId,
            riderId = Kernel.Types.Id.Id riderId,
            updatedAt = updatedAt,
            wasOfferedAssistance = wasOfferedAssistance,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Rating Domain.Types.Rating.Rating where
  toTType' (Domain.Types.Rating.Rating {..}) = do
    Beam.RatingT
      { Beam.createdAt = createdAt,
        Beam.feedbackDetails = feedbackDetails,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.mediaId = Kernel.Types.Id.getId <$> mediaId,
        Beam.ratingValue = ratingValue,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.updatedAt = updatedAt,
        Beam.wasOfferedAssistance = wasOfferedAssistance,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
