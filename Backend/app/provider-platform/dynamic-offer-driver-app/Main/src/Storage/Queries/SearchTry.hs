{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchTry where

import qualified Database.Beam.Query ()
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry as Domain
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchTry as BeamST

-- import qualified Storage.Beam.SearchTry as BeamST

create :: L.MonadFlow m => SearchTry -> m (MeshResult ())
create searchTry = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainSearchTryToBeam searchTry)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: L.MonadFlow m => Id SearchTry -> m (Maybe SearchTry)
findById (Id searchTry) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamSearchTryToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamST.id $ Se.Eq searchTry]
    Nothing -> pure Nothing

findLastByRequestId ::
  L.MonadFlow m =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findLastByRequestId (Id searchRequest) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      _ <- do
        result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig [Se.Is BeamST.id $ Se.Eq searchRequest] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing
        case result of
          Left _ -> pure Nothing
          Right val' ->
            let searchtries = transformBeamSearchTryToDomain <$> val'
             in pure $ headMaybe searchtries
      pure Nothing
    Nothing -> pure Nothing
  where
    headMaybe [] = Nothing
    headMaybe (a : _) = Just a

findActiveTryByRequestId ::
  L.MonadFlow m =>
  Id SearchRequest ->
  m (Maybe SearchTry)
findActiveTryByRequestId (Id searchRequest) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithOptionsKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamST.id $ Se.Eq searchRequest, Se.Is BeamST.status $ Se.Eq ACTIVE]] (Se.Desc BeamST.searchRepeatCounter) (Just 1) Nothing
      case result of
        Right (searchTry : _) ->
          pure $ Just $ transformBeamSearchTryToDomain searchTry
        _ -> pure Nothing
    Nothing -> pure Nothing

cancelActiveTriesByRequestId ::
  (L.MonadFlow m, MonadTime m) =>
  Id SearchRequest ->
  m (MeshResult ())
cancelActiveTriesByRequestId (Id searchId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamST.status CANCELLED,
          Se.Set BeamST.updatedAt now
        ]
        [ Se.And
            [ Se.Is BeamST.requestId $ Se.Eq searchId,
              Se.Is BeamST.status $ Se.Eq ACTIVE
            ]
        ]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

updateStatus ::
  (L.MonadFlow m, MonadTime m) =>
  Id SearchTry ->
  SearchTryStatus ->
  m ()
updateStatus (Id searchId) status_ = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamST.status status_,
            Se.Set BeamST.updatedAt now
          ]
          [Se.Is BeamST.id $ Se.Eq searchId]
    Nothing -> pure ()

getSearchTryStatusAndValidTill ::
  (L.MonadFlow m) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchTryStatusAndValidTill (Id searchRequestId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamST.SearchTryT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamST.id $ Se.Eq searchRequestId]
      case result of
        Left _ -> pure Nothing
        Right val' -> do
          let searchTries = transformBeamSearchTryToDomain <$> val'
          let validTill = Domain.validTill <$> searchTries
          let status = Domain.status <$> searchTries
          pure $ (,) <$> validTill <*> status
    Nothing -> pure Nothing

transformBeamSearchTryToDomain :: BeamST.SearchTry -> SearchTry
transformBeamSearchTryToDomain BeamST.SearchTryT {..} = do
  SearchTry
    { id = Id id,
      requestId = Id requestId,
      estimateId = Id estimateId,
      merchantId = Id <$> merchantId,
      messageId = messageId,
      startTime = startTime,
      validTill = validTill,
      vehicleVariant = vehicleVariant,
      baseFare = baseFare,
      customerExtraFee = customerExtraFee,
      status = status,
      searchRepeatCounter = searchRepeatCounter,
      searchRepeatType = searchRepeatType,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainSearchTryToBeam :: SearchTry -> BeamST.SearchTry
transformDomainSearchTryToBeam SearchTry {..} =
  BeamST.SearchTryT
    { id = getId id,
      requestId = getId requestId,
      estimateId = getId estimateId,
      merchantId = getId <$> merchantId,
      messageId = messageId,
      startTime = startTime,
      validTill = validTill,
      vehicleVariant = vehicleVariant,
      baseFare = baseFare,
      customerExtraFee = customerExtraFee,
      status = status,
      searchRepeatCounter = searchRepeatCounter,
      searchRepeatType = searchRepeatType,
      createdAt = createdAt,
      updatedAt = updatedAt
    }
