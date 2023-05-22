{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Rating where

import Domain.Types.Person
import Domain.Types.Rating as DR
import Domain.Types.Ride
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Rating as BeamR
import Storage.Tabular.Rating
import qualified Storage.Tabular.VechileNew as VN

create :: Rating -> SqlDB ()
create = Esq.create

create' :: L.MonadFlow m => DR.Rating -> m (MeshResult ())
create' rating = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainRatingToBeam rating)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

updateRating :: Id Rating -> Id Person -> Int -> Maybe Text -> SqlDB ()
updateRating ratingId driverId newRatingValue newFeedbackDetails = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RatingRatingValue =. val newRatingValue,
        RatingFeedbackDetails =. val newFeedbackDetails,
        RatingUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RatingTId ==. val (toKey ratingId)
        &&. tbl ^. RatingDriverId ==. val (toKey driverId)

updateRating' :: (L.MonadFlow m, MonadTime m) => Id Rating -> Id Person -> Int -> Maybe Text -> m (MeshResult ())
updateRating' (Id ratingId) (Id driverId) newRatingValue newFeedbackDetails = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamR.ratingValue newRatingValue,
          Se.Set BeamR.feedbackDetails newFeedbackDetails,
          Se.Set BeamR.updatedAt now
        ]
        [Se.And [Se.Is BeamR.id (Se.Eq ratingId), Se.Is BeamR.driverId (Se.Eq driverId)]]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

findAllRatingsForPerson :: Transactionable m => Id Person -> m [Rating]
findAllRatingsForPerson driverId =
  findAll $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
    return rating

findAllRatingsForPerson' :: L.MonadFlow m => Id Person -> m [Rating]
findAllRatingsForPerson' driverId = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamRatingToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamR.driverId $ Se.Eq $ getId driverId]
    Nothing -> pure []

findRatingForRide :: Transactionable m => Id Ride -> m (Maybe Rating)
findRatingForRide rideId = findOne $ do
  rating <- from $ table @RatingT
  where_ $ rating ^. RatingRideId ==. val (toKey rideId)
  pure rating

findRatingForRide' :: L.MonadFlow m => Id Ride -> m (Maybe Rating)
findRatingForRide' (Id rideId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamRatingToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamR.id $ Se.Eq rideId]
    Nothing -> pure Nothing

transformBeamRatingToDomain :: BeamR.Rating -> Rating
transformBeamRatingToDomain BeamR.RatingT {..} = do
  Rating
    { id = Id id,
      rideId = Id rideId,
      driverId = Id driverId,
      ratingValue = ratingValue,
      feedbackDetails = feedbackDetails,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainRatingToBeam :: Rating -> BeamR.Rating
transformDomainRatingToBeam Rating {..} =
  BeamR.defaultRating
    { BeamR.id = getId id,
      BeamR.rideId = getId rideId,
      BeamR.driverId = getId driverId,
      BeamR.ratingValue = ratingValue,
      BeamR.feedbackDetails = feedbackDetails,
      BeamR.createdAt = createdAt,
      BeamR.updatedAt = updatedAt
    }
