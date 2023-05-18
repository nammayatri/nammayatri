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
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestSpecialZone as BeamSRSZ
import Storage.Tabular.SearchRequest.SearchReqLocation
import Storage.Tabular.SearchRequestSpecialZone
import qualified Storage.Tabular.VechileNew as VN

create :: SearchRequestSpecialZone -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: Transactionable m => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
findById searchRequestSpecialZoneId = buildDType $
  fmap (fmap $ extractSolidType @Domain.SearchRequestSpecialZone) $
    Esq.findOne' $ do
      (sReq :& sFromLoc :& sToLoc) <-
        from
          ( table @SearchRequestSpecialZoneT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestSpecialZoneFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestSpecialZoneToLocationId ==. loc2 ^. SearchReqLocationTId)
          )
      where_ $ sReq ^. SearchRequestSpecialZoneTId ==. val (toKey searchRequestSpecialZoneId)
      pure (sReq, sFromLoc, sToLoc)

fullSearchRequestTable ::
  From
    ( Table SearchRequestSpecialZoneT
        :& Table SearchReqLocationT
        :& Table SearchReqLocationT
    )
fullSearchRequestTable =
  table @SearchRequestSpecialZoneT
    `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchRequestSpecialZoneFromLocationId ==. loc1 ^. SearchReqLocationTId)
    `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchRequestSpecialZoneToLocationId ==. loc2 ^. SearchReqLocationTId)

getRequestIdfromTransactionId ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe (Id SearchRequestSpecialZone))
getRequestIdfromTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchT ^. SearchRequestSpecialZoneTransactionId ==. val (getId tId)
    return $ searchT ^. SearchRequestSpecialZoneTId

findByMsgIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByMsgIdAndBapIdAndBppId txnId bapId merchantId = Esq.buildDType $ do
  mbFullSearchReqT <- Esq.findOne' $ do
    (sReq :& sFromLoc :& mbSToLoc) <- from fullSearchRequestTable
    where_ $
      sReq ^. SearchRequestSpecialZoneMessageId ==. val txnId
        &&. sReq ^. SearchRequestSpecialZoneProviderId ==. val (toKey merchantId)
        &&. sReq ^. SearchRequestSpecialZoneBapId ==. val bapId
    pure (sReq, sFromLoc, mbSToLoc)
  pure $ extractSolidType @SearchRequestSpecialZone <$> mbFullSearchReqT

getValidTill ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe UTCTime)
getValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchT ^. SearchRequestSpecialZoneTId ==. val (toKey searchRequestId)
    return $ searchT ^. SearchRequestSpecialZoneValidTill

-- transformBeamSearchRequestSpecialZoneToDomain :: BeamSRSZ.SearchRequestSpecialZone -> SearchRequestSpecialZone
-- transformBeamSearchRequestSpecialZoneToDomain BeamSRSZ.SearchRequestSpecialZoneT {..} = do
--   SearchRequestSpecialZone
--     { id = Id id,
--       transactionId = transactionId,
--       messageId = messageId,
--       startTime = startTime,
--       validTill = validTill,
--       providerId = Id providerId,
--       fromLocation = fromLocation,
--       toLocation = toLocation,
--       bapId = bapId,
--       bapUri = bapUri,
--       estimatedDistance = estimatedDistance,
--       estimatedDuration = estimatedDuration,
--       createdAt = createdAt,
--       updatedAt = updatedAt
--     }

-- transformDomainSearchRequestSpecialZoneToBeam :: SearchRequestSpecialZone -> BeamSRSZ.SearchRequestSpecialZone
-- transformDomainSearchRequestSpecialZoneToBeam SearchRequestSpecialZone {..} =
--   BeamSRSZ.defaultSearchRequestSpecialZone
--     { BeamSRSZ.id = getId id,
--       BeamSRSZ.transactionId = transactionId,
--       BeamSRSZ.messageId = messageId,
--       BeamSRSZ.startTime = startTime,
--       BeamSRSZ.validTill = validTill,
--       BeamSRSZ.providerId = getId providerId,
--       BeamSRSZ.fromLocationId = fromLocation,
--       BeamSRSZ.toLocationId = toLocation,
--       BeamSRSZ.bapId = bapId,
--       BeamSRSZ.bapUri = showBaseUrl bapUri,
--       BeamSRSZ.estimatedDistance = estimatedDistance,
--       BeamSRSZ.estimatedDuration = estimatedDuration,
--       BeamSRSZ.createdAt = createdAt,
--       BeamSRSZ.updatedAt = updatedAt
--     }
