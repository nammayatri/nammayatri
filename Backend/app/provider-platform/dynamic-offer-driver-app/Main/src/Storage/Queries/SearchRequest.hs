{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequest where

import Domain.Types.SearchRequest as Domain
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as BeamSR
import Storage.Queries.SearchRequest.SearchReqLocation as QSRL

createDSReq :: L.MonadFlow m => SearchRequest -> m (MeshResult ())
createDSReq sReq = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSR.SearchRequestT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainSearchRequestToBeam sReq)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

create :: L.MonadFlow m => SearchRequest -> m (MeshResult ())
create dsReq = do
  _ <- createDSReq dsReq
  _ <- QSRL.create dsReq.fromLocation
  QSRL.create dsReq.toLocation

findById :: L.MonadFlow m => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSR.SearchRequestT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      sR <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamSR.id $ Se.Eq searchRequestId]
      case sR of
        Right (Just x) -> transformBeamSearchRequestToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

getRequestIdfromTransactionId :: L.MonadFlow m => Id SearchRequest -> m (Maybe (Id SearchRequest))
getRequestIdfromTransactionId (Id tId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSR.SearchRequestT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      sr <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamSR.transactionId $ Se.Eq tId]
      case sr of
        Right (Just x) -> do
          sr' <- transformBeamSearchRequestToDomain x
          let srId = Domain.id <$> sr'
          pure srId
        _ -> pure Nothing
    Nothing -> pure Nothing

findByTransactionId :: L.MonadFlow m => Text -> m (Maybe (Id SearchRequest))
findByTransactionId transactionId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSR.SearchRequestT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> do
      sr <- KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamSR.transactionId $ Se.Eq transactionId]]
      case sr of
        Right (Just x) -> do
          sr' <- transformBeamSearchRequestToDomain x
          let srId = Domain.id <$> sr'
          pure srId
        _ -> pure Nothing
    Nothing -> pure Nothing

updateAutoAssign ::
  L.MonadFlow m =>
  Id SearchRequest ->
  Bool ->
  m ()
updateAutoAssign searchRequestId autoAssignedEnabled = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamSR.SearchRequestT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled]
          [Se.Is BeamSR.id (Se.Eq $ getId searchRequestId)]
    Nothing -> pure ()

transformBeamSearchRequestToDomain :: L.MonadFlow m => BeamSR.SearchRequest -> m (Maybe SearchRequest)
transformBeamSearchRequestToDomain BeamSR.SearchRequestT {..} = do
  fl <- QSRL.findById (Id fromLocationId)
  tl <- QSRL.findById (Id toLocationId)
  pUrl <- parseBaseUrl bapUri
  if isJust fl && isJust tl
    then
      pure $
        Just
          SearchRequest
            { id = Id id,
              transactionId = transactionId,
              providerId = Id providerId,
              fromLocation = fromJust fl,
              toLocation = fromJust tl,
              area = area,
              bapId = bapId,
              bapUri = pUrl,
              estimatedDistance = estimatedDistance,
              estimatedDuration = estimatedDuration,
              customerLanguage = customerLanguage,
              device = device,
              createdAt = createdAt,
              specialLocationTag = specialLocationTag,
              autoAssignEnabled = autoAssignEnabled
            }
    else pure Nothing

transformDomainSearchRequestToBeam :: SearchRequest -> BeamSR.SearchRequest
transformDomainSearchRequestToBeam SearchRequest {..} =
  BeamSR.SearchRequestT
    { BeamSR.id = getId id,
      BeamSR.transactionId = transactionId,
      BeamSR.providerId = getId providerId,
      BeamSR.fromLocationId = getId fromLocation.id,
      BeamSR.toLocationId = getId toLocation.id,
      BeamSR.area = area,
      BeamSR.bapId = bapId,
      BeamSR.bapUri = showBaseUrl bapUri,
      BeamSR.estimatedDistance = estimatedDistance,
      BeamSR.estimatedDuration = estimatedDuration,
      BeamSR.customerLanguage = customerLanguage,
      BeamSR.device = device,
      BeamSR.createdAt = createdAt,
      BeamSR.autoAssignEnabled = autoAssignEnabled,
      BeamSR.specialLocationTag = specialLocationTag
    }

-- updateAutoAssign ::
--   Id SearchRequest ->
--   Bool ->
--   SqlDB ()
-- updateAutoAssign searchRequestId autoAssignedEnabled = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ SearchRequestAutoAssignEnabled =. val (Just autoAssignedEnabled)
--       ]
--     where_ $ tbl ^. SearchRequestTId ==. val (toKey searchRequestId)
