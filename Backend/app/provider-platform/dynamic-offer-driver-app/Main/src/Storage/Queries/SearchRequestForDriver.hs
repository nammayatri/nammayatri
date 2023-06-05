{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequestForDriver where

import Domain.Types.Person
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRequestForDriver as Domain
import Domain.Types.SearchTry
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestForDriver as BeamSRFD
import Storage.Tabular.SearchRequestForDriver

createMany :: L.MonadFlow m => [SearchRequestForDriver] -> m ()
createMany srfd = void $ traverse createOne srfd
  where
    createOne :: L.MonadFlow m => SearchRequestForDriver -> m ()
    createOne searchRequestForDriver = do
      dbConf <- L.getOption Extra.EulerPsqlDbCfg
      case dbConf of
        Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainSearchRequestForDriverToBeam searchRequestForDriver)
        Nothing -> pure ()

findAllActiveBySTId :: L.MonadFlow m => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveBySTId (Id searchTryId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [ Se.And
              [ Se.Is BeamSRFD.id $ Se.Eq searchTryId,
                Se.Is BeamSRFD.status $ Se.Eq Domain.Active
              ]
          ]
    Nothing -> pure []

findAllActiveBySRId :: L.MonadFlow m => Id SearchRequest -> m [SearchRequestForDriver]
findAllActiveBySRId (Id searchReqId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [ Se.And
              [ Se.Is BeamSRFD.requestId $ Se.Eq searchReqId,
                Se.Is BeamSRFD.status $ Se.Eq Domain.Active
              ]
          ]
    Nothing -> pure []

-- findAllActiveBySTId :: (Transactionable m, MonadTime m) => Id SearchTry -> m [SearchRequestForDriver]
-- findAllActiveBySTId searchTryId = do
--   Esq.findAll $ do
--     sReq <- from $ table @SearchRequestForDriverT
--     where_ $
--       sReq ^. SearchRequestForDriverSearchTryId ==. val (toKey searchTryId)
--         &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
--     pure sReq

findAllActiveWithoutRespBySearchTryId :: (Transactionable m, MonadTime m) => Id SearchTry -> m [SearchRequestForDriver]
findAllActiveWithoutRespBySearchTryId searchTryId = do
  Esq.findAll $ do
    sReq <- from $ table @SearchRequestForDriverT
    where_ $
      sReq ^. SearchRequestForDriverSearchTryId ==. val (toKey searchTryId)
        &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
        &&. Esq.isNothing (sReq ^. SearchRequestForDriverResponse)
    pure sReq

findByDriverAndSearchTryId :: Transactionable m => Id Person -> Id SearchTry -> m (Maybe SearchRequestForDriver)
findByDriverAndSearchTryId driverId searchTryId = Esq.findOne $ do
  sReq <- from $ table @SearchRequestForDriverT
  where_ $
    sReq ^. SearchRequestForDriverSearchTryId ==. val (toKey searchTryId)
      &&. sReq ^. SearchRequestForDriverDriverId ==. val (toKey driverId)
      &&. sReq ^. SearchRequestForDriverStatus ==. val Domain.Active
  pure sReq

findByDriver :: (L.MonadFlow m, MonadTime m) => Id Person -> m [SearchRequestForDriver]
findByDriver (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbCOnf' ->
      either (pure []) (transformBeamSearchRequestForDriverToDomain <$>)
        <$> KV.findAllWithKVConnector
          dbCOnf'
          Mesh.meshConfig
          [Se.And [Se.Is BeamSRFD.driverId $ Se.Eq driverId, Se.Is BeamSRFD.status $ Se.Eq Domain.Active, Se.Is BeamSRFD.searchRequestValidTill $ Se.GreaterThan now]]
    Nothing -> pure []

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamSRFD.driverId (Se.Eq personId)]
    Nothing -> pure ()

-- setInactiveBySTId :: Id SearchTry -> SqlDB ()
-- setInactiveBySTId searchTryId = Esq.update $ \p -> do
--   set p [SearchRequestForDriverStatus =. val Domain.Inactive]
--   where_ $ p ^. SearchRequestForDriverSearchTryId ==. val (toKey searchTryId)

setInactiveBySTId :: L.MonadFlow m => Id SearchTry -> m (MeshResult ())
setInactiveBySTId (Id searchTryId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [Se.Set BeamSRFD.status Domain.Inactive]
        [Se.Is BeamSRFD.searchTryId (Se.Eq searchTryId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

setInactiveBySRId :: L.MonadFlow m => Id SearchRequest -> m (MeshResult ())
setInactiveBySRId (Id searchReqId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [Se.Set BeamSRFD.status Domain.Inactive]
        [Se.Is BeamSRFD.requestId (Se.Eq searchReqId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateDriverResponse :: L.MonadFlow m => Id SearchRequestForDriver -> SearchRequestForDriverResponse -> m (MeshResult ())
updateDriverResponse (Id id) response = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [Se.Set BeamSRFD.response (Just response)]
        [Se.Is BeamSRFD.id (Se.Eq id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamSearchRequestForDriverToDomain :: BeamSRFD.SearchRequestForDriver -> SearchRequestForDriver
transformBeamSearchRequestForDriverToDomain BeamSRFD.SearchRequestForDriverT {..} = do
  SearchRequestForDriver
    { id = Id id,
      requestId = Id requestId,
      searchTryId = Id searchTryId,
      startTime = startTime,
      searchRequestValidTill = searchRequestValidTill,
      driverId = Id driverId,
      actualDistanceToPickup = actualDistanceToPickup,
      straightLineDistanceToPickup = straightLineDistanceToPickup,
      durationToPickup = durationToPickup,
      vehicleVariant = vehicleVariant,
      status = status,
      batchNumber = batchNumber,
      lat = lat,
      lon = lon,
      createdAt = createdAt,
      response = response,
      driverMinExtraFee = driverMinExtraFee,
      driverMaxExtraFee = driverMaxExtraFee,
      rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
      isPartOfIntelligentPool = isPartOfIntelligentPool,
      cancellationRatio = cancellationRatio,
      acceptanceRatio = acceptanceRatio,
      driverAvailableTime = driverAvailableTime,
      parallelSearchRequestCount = parallelSearchRequestCount,
      driverSpeed = driverSpeed,
      mode = mode
    }

transformDomainSearchRequestForDriverToBeam :: SearchRequestForDriver -> BeamSRFD.SearchRequestForDriver
transformDomainSearchRequestForDriverToBeam SearchRequestForDriver {..} =
  BeamSRFD.SearchRequestForDriverT
    { BeamSRFD.id = getId id,
      BeamSRFD.requestId = getId requestId,
      BeamSRFD.searchTryId = getId searchTryId,
      BeamSRFD.startTime = startTime,
      BeamSRFD.searchRequestValidTill = searchRequestValidTill,
      BeamSRFD.driverId = getId driverId,
      BeamSRFD.actualDistanceToPickup = actualDistanceToPickup,
      BeamSRFD.straightLineDistanceToPickup = straightLineDistanceToPickup,
      BeamSRFD.durationToPickup = durationToPickup,
      BeamSRFD.vehicleVariant = vehicleVariant,
      BeamSRFD.status = status,
      BeamSRFD.batchNumber = batchNumber,
      BeamSRFD.lat = lat,
      BeamSRFD.lon = lon,
      BeamSRFD.createdAt = createdAt,
      BeamSRFD.response = response,
      BeamSRFD.driverMinExtraFee = driverMinExtraFee,
      BeamSRFD.driverMaxExtraFee = driverMaxExtraFee,
      BeamSRFD.rideRequestPopupDelayDuration = rideRequestPopupDelayDuration,
      BeamSRFD.isPartOfIntelligentPool = isPartOfIntelligentPool,
      BeamSRFD.cancellationRatio = cancellationRatio,
      BeamSRFD.acceptanceRatio = acceptanceRatio,
      BeamSRFD.driverAvailableTime = driverAvailableTime,
      BeamSRFD.parallelSearchRequestCount = parallelSearchRequestCount,
      BeamSRFD.driverSpeed = driverSpeed,
      BeamSRFD.mode = mode
    }
