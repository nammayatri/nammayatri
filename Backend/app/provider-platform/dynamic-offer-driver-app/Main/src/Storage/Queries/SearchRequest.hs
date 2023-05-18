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
import qualified Storage.Beam.SearchRequest as BeamSR
import Storage.Tabular.SearchRequest
import Storage.Tabular.SearchRequest.SearchReqLocation
import qualified Storage.Tabular.VechileNew as VN

create :: SearchRequest -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = buildDType $
  fmap (fmap $ extractSolidType @Domain.SearchRequest) $
    Esq.findOne' $ do
      (sReq :& sFromLoc :& sToLoc) <-
        from
          ( table @SearchRequestT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestToLocationId ==. loc2 ^. SearchReqLocationTId)
          )
      where_ $ sReq ^. SearchRequestTId ==. val (toKey searchRequestId)
      pure (sReq, sFromLoc, sToLoc)

updateStatus ::
  Id SearchRequest ->
  SearchRequestStatus ->
  SqlDB ()
updateStatus searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestUpdatedAt =. val now,
        SearchRequestStatus =. val status_
      ]
    where_ $ tbl ^. SearchRequestTId ==. val (toKey searchId)

-- Overlapping instances error
-- updateStatus' :: (L.MonadFlow m, MonadTime m) => Id SearchRequest -> SearchRequestStatus -> m (MeshResult ())
-- updateStatus' (Id searchId) status_ = do
--   dbConf <- L.getOption Extra.EulerPsqlDbCfg
--   now <- getCurrentTime
--   case dbConf of
--     Just dbConf' ->
--       KV.updateWoReturningWithKVConnector
--         dbConf'
--         VN.meshConfig
--         [ Se.Set BeamSR.status status_,
--           Se.Set BeamSR.updatedAt now
--         ]
--         [Se.Is BeamSR.id (Se.Eq searchId)]
--     Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

getRequestIdfromTransactionId ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe (Id SearchRequest))
getRequestIdfromTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTransactionId ==. val (getId tId)
    return $ searchT ^. SearchRequestTId

getSearchRequestStatusOrValidTill ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe (UTCTime, SearchRequestStatus))
getSearchRequestStatusOrValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTId ==. val (toKey searchRequestId)
    return (searchT ^. SearchRequestValidTill, searchT ^. SearchRequestStatus)

findActiveByTransactionId ::
  (Transactionable m) =>
  Text ->
  m (Maybe (Id SearchRequest))
findActiveByTransactionId transactionId = do
  findOne $ do
    searchT <- from $ table @SearchRequestT
    where_ $
      searchT ^. SearchRequestTransactionId ==. val transactionId
        &&. searchT ^. SearchRequestStatus ==. val Domain.ACTIVE
    return $ searchT ^. SearchRequestTId

-- transformBeamSearchRequestToDomain :: BeamSR.SearchRequest -> SearchRequest
-- transformBeamSearchRequestToDomain BeamSR.SearchRequestT {..} = do
--   SearchRequest
--     { id = Id id,
--       estimateId = Id estimateId,
--       transactionId = transactionId,
--       messageId = messageId,
--       startTime = startTime,
--       validTill = validTill,
--       providerId = Id providerId,
--       fromLocation = fromLocation,
--       toLocation = toLocation,
--       bapId = bapId,
--       bapUri = pUrl,
--       estimatedDistance = estimatedDistance,
--       estimatedDuration = estimatedDuration,
--       customerExtraFee = customerExtraFee,
--       device = device,
--       createdAt = createdAt,
--       updatedAt = updatedAt,
--       vehicleVariant = vehicleVariant,
--       status = status,
--       autoAssignEnabled = autoAssignEnabled,
--       searchRepeatCounter = searchRepeatCounter
--     }

transformDomainSearchRequestToBeam :: SearchRequest -> BeamSR.SearchRequest
transformDomainSearchRequestToBeam SearchRequest {..} =
  BeamSR.defaultSearchRequest
    { BeamSR.id = getId id,
      BeamSR.estimateId = getId estimateId,
      BeamSR.transactionId = transactionId,
      BeamSR.messageId = messageId,
      BeamSR.startTime = startTime,
      BeamSR.validTill = validTill,
      BeamSR.providerId = getId providerId,
      BeamSR.fromLocationId = getId fromLocation.id,
      BeamSR.toLocationId = getId toLocation.id,
      BeamSR.bapId = bapId,
      BeamSR.bapUri = showBaseUrl bapUri,
      BeamSR.estimatedDistance = estimatedDistance,
      BeamSR.estimatedDuration = estimatedDuration,
      BeamSR.customerExtraFee = customerExtraFee,
      BeamSR.device = device,
      BeamSR.createdAt = createdAt,
      BeamSR.updatedAt = updatedAt,
      BeamSR.vehicleVariant = vehicleVariant,
      BeamSR.status = status,
      BeamSR.autoAssignEnabled = autoAssignEnabled,
      BeamSR.searchRepeatCounter = searchRepeatCounter
    }
