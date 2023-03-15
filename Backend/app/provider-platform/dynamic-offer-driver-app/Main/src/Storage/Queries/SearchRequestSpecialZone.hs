{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequestSpecialZone where

import Domain.Types.SearchRequestSpecialZone as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Domain.Types.Merchant
import Kernel.Types.Id
import Storage.Tabular.SearchRequestSpecialZone
import Storage.Tabular.SearchRequest.SearchReqLocation

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
