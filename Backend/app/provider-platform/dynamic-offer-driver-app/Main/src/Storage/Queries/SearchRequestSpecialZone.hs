{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchRequestSpecialZone where

import Domain.Types.Merchant
import Domain.Types.SearchRequestSpecialZone as Domain
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
-- import Kernel.Storage.Esqueleto as Esq

import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestSpecialZone as BeamSRSZ
import Storage.Queries.SearchRequest.SearchReqLocation as QSRL
-- import Storage.Tabular.SearchRequestSpecialZone

-- import Storage.Tabular.SearchRequest.SearchReqLocation
import Storage.Tabular.SearchRequestSpecialZone

-- create :: SearchRequestSpecialZone -> SqlDB ()
-- create dsReq = Esq.runTransaction $
--   withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
--     Esq.create' fromLoc
--     Esq.create' toLoc
--     Esq.create' sReq

createSearchRequestSpecialZone :: L.MonadFlow m => SearchRequestSpecialZone -> m (MeshResult ())
createSearchRequestSpecialZone srsz = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainSearchRequestSpecialZoneToBeam srsz)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

create :: L.MonadFlow m => SearchRequestSpecialZone -> m (MeshResult ())
create srsz = do
  _ <- createSearchRequestSpecialZone srsz
  _ <- QSRL.create srsz.fromLocation
  QSRL.create srsz.toLocation

-- findById :: Transactionable m => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
-- findById searchRequestSpecialZoneId = buildDType $
--   fmap (fmap $ extractSolidType @Domain.SearchRequestSpecialZone) $
--     Esq.findOne' $ do
--       (sReq :& sFromLoc :& sToLoc) <-
--         from
--           ( table @SearchRequestSpecialZoneT
--               `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestSpecialZoneFromLocationId ==. loc1 ^. SearchReqLocationTId)
--               `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestSpecialZoneToLocationId ==. loc2 ^. SearchReqLocationTId)
--           )
--       where_ $ sReq ^. SearchRequestSpecialZoneTId ==. val (toKey searchRequestSpecialZoneId)
--       pure (sReq, sFromLoc, sToLoc)
-- We are already finding SearchRequestLocation in Domain transform function.
findById :: L.MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
findById (Id searchRequestSpecialZoneId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      sR <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamSRSZ.id $ Se.Eq searchRequestSpecialZoneId]
      case sR of
        Right (Just x) -> transformBeamSearchRequestSpecialZoneToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

-- fullSearchRequestTable ::
--   From
--     ( Table SearchRequestSpecialZoneT
--         :& Table SearchReqLocationT
--         :& Table SearchReqLocationT
--     )
-- fullSearchRequestTable =
--   table @SearchRequestSpecialZoneT
--     `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestSpecialZoneFromLocationId ==. loc1 ^. SearchReqLocationTId)
--     `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestSpecialZoneToLocationId ==. loc2 ^. SearchReqLocationTId)

-- getRequestIdfromTransactionId ::
--   (Transactionable m) =>
--   Id SearchRequestSpecialZone ->
--   m (Maybe (Id SearchRequestSpecialZone))
-- getRequestIdfromTransactionId tId = do
--   findOne $ do
--     searchT <- from $ table @SearchRequestSpecialZoneT
--     where_ $
--       searchT ^. SearchRequestSpecialZoneTransactionId ==. val (getId tId)
--     return $ searchT ^. SearchRequestSpecialZoneTId

getRequestIdfromTransactionId :: L.MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe (Id SearchRequestSpecialZone))
getRequestIdfromTransactionId (Id tId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamSRSZ.transactionId $ Se.Eq tId]
      case srsz of
        Right (Just x) -> do
          srsz' <- transformBeamSearchRequestSpecialZoneToDomain x
          let vTill = Domain.id <$> srsz'
          pure vTill
        _ -> pure Nothing
    Nothing -> pure Nothing

-- findByMsgIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
-- findByMsgIdAndBapIdAndBppId txnId bapId merchantId = Esq.buildDType $ do
--   mbFullSearchReqT <- Esq.findOne' $ do
--     (sReq :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
--     where_ $
--       sReq ^. SearchRequestSpecialZoneMessageId ==. val txnId
--         &&. sReq ^. SearchRequestSpecialZoneProviderId ==. val (toKey merchantId)
--         &&. sReq ^. SearchRequestSpecialZoneBapId ==. val bapId
--     pure (sReq, sFromLoc, mbSToLoc)
--   pure $ extractSolidType @SearchRequestSpecialZone <$> mbFullSearchReqT

findByMsgIdAndBapIdAndBppId :: L.MonadFlow m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByMsgIdAndBapIdAndBppId txnId bapId (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamSRSZ.messageId $ Se.Eq txnId, Se.Is BeamSRSZ.providerId $ Se.Eq merchantId, Se.Is BeamSRSZ.bapId $ Se.Eq bapId]]
      case srsz of
        Right (Just x) -> transformBeamSearchRequestSpecialZoneToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

-- getValidTill ::
--   (Transactionable m) =>
--   Id SearchRequestSpecialZone ->
--   m (Maybe UTCTime)
-- getValidTill searchRequestId = do
--   findOne $ do
--     searchT <- from $ table @SearchRequestSpecialZoneT
--     where_ $
--       searchT ^. SearchRequestSpecialZoneTId ==. val (toKey searchRequestId)
--     return $ searchT ^. SearchRequestSpecialZoneValidTill
findByTransactionId ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe (Id SearchRequestSpecialZone))
findByTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchT ^. SearchRequestSpecialZoneTransactionId ==. val (getId tId)
    return $ searchT ^. SearchRequestSpecialZoneTId

findByTransactionId' ::
  (L.MonadFlow m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe (Id SearchRequestSpecialZone))
findByTransactionId' (Id tId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamSRSZ.transactionId $ Se.Eq tId]
      case srsz of
        Right (Just x) -> do
          srsz' <- transformBeamSearchRequestSpecialZoneToDomain x
          case srsz' of
            Just val' -> pure $ Just $ Domain.id val'
            Nothing -> pure Nothing
        _ -> pure Nothing
    Nothing -> pure Nothing

getValidTill :: L.MonadFlow m => Id SearchRequestSpecialZone -> m (Maybe UTCTime)
getValidTill (Id searchRequestId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      srsz <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamSRSZ.id $ Se.Eq searchRequestId]
      case srsz of
        Right (Just x) -> do
          srsz' <- transformBeamSearchRequestSpecialZoneToDomain x
          let vTill = Domain.validTill <$> srsz'
          pure vTill
        _ -> pure Nothing
    Nothing -> pure Nothing

transformBeamSearchRequestSpecialZoneToDomain :: L.MonadFlow m => BeamSRSZ.SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
transformBeamSearchRequestSpecialZoneToDomain BeamSRSZ.SearchRequestSpecialZoneT {..} = do
  fl <- QSRL.findById (Id fromLocationId)
  tl <- QSRL.findById (Id toLocationId)
  pUrl <- parseBaseUrl bapUri
  if isJust fl && isJust tl
    then
      pure $
        Just
          SearchRequestSpecialZone
            { id = Id id,
              transactionId = transactionId,
              messageId = messageId,
              startTime = startTime,
              validTill = validTill,
              providerId = Id providerId,
              fromLocation = fromJust fl,
              toLocation = fromJust tl,
              bapId = bapId,
              bapUri = pUrl,
              estimatedDistance = estimatedDistance,
              estimatedDuration = estimatedDuration,
              createdAt = createdAt,
              updatedAt = updatedAt
            }
    else pure Nothing

transformDomainSearchRequestSpecialZoneToBeam :: SearchRequestSpecialZone -> BeamSRSZ.SearchRequestSpecialZone
transformDomainSearchRequestSpecialZoneToBeam SearchRequestSpecialZone {..} =
  BeamSRSZ.SearchRequestSpecialZoneT
    { BeamSRSZ.id = getId id,
      BeamSRSZ.transactionId = transactionId,
      BeamSRSZ.messageId = messageId,
      BeamSRSZ.startTime = startTime,
      BeamSRSZ.validTill = validTill,
      BeamSRSZ.providerId = getId providerId,
      BeamSRSZ.fromLocationId = getId fromLocation.id,
      BeamSRSZ.toLocationId = getId toLocation.id,
      BeamSRSZ.bapId = bapId,
      BeamSRSZ.bapUri = showBaseUrl bapUri,
      BeamSRSZ.estimatedDistance = estimatedDistance,
      BeamSRSZ.estimatedDuration = estimatedDuration,
      BeamSRSZ.createdAt = createdAt,
      BeamSRSZ.updatedAt = updatedAt
    }
