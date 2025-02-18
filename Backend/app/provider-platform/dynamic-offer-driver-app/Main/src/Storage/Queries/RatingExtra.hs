module Storage.Queries.RatingExtra where

-- Extra code goes here --

import qualified Database.Beam as B
import Domain.Types.Person (Person)
import qualified Domain.Types.Rating
import qualified Domain.Types.Ride
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Rating as Beam
import qualified Storage.Beam.Rating as BeamR
import Storage.Queries.OrphanInstances.Rating ()

findAllRatingUsersCountByPerson :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Types.Person.Person -> m Int
findAllRatingUsersCountByPerson (Id driverId) = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            (\rating' -> BeamR.driverId rating' B.==?. B.val_ driverId)
            do
              B.all_ (BeamCommon.rating BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

checkIfRatingExistsForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Types.Person.Person -> m [Domain.Types.Rating.Rating]
checkIfRatingExistsForDriver (Id driverId) = do findAllWithOptionsKV' [Se.Is Beam.driverId $ Se.Eq driverId] (Just 1) Nothing

findTopRatingsForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Types.Person.Person -> Maybe Int -> m [Domain.Types.Rating.Rating]
findTopRatingsForDriver (Id driverId) limit = do
  findAllWithOptionsKV'
    [ Se.Is Beam.driverId $ Se.Eq driverId,
      Se.Is Beam.ratingValue $ Se.In [4, 5],
      Se.Is Beam.feedbackDetails $ Se.Not $ Se.Eq (Just ""),
      Se.Is Beam.feedbackDetails $ Se.Not $ Se.Eq Nothing
    ]
    limit
    Nothing

findRatingForRideIfPositive :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Domain.Types.Ride.Ride] -> m [Domain.Types.Rating.Rating]
findRatingForRideIfPositive rideIds = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.rideId $ Se.In (getId <$> rideIds),
          Se.Is Beam.ratingValue $ Se.In [4, 5]
        ]
    ]
