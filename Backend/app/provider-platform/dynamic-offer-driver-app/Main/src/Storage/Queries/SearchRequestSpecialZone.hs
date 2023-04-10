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
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SearchRequest.SearchReqLocation
import Storage.Tabular.SearchRequestSpecialZone

create :: SearchRequestSpecialZone -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

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

-- queries fetching only one entity should avoid join for performance reason

findById :: Transactionable m => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
findById searchRequestId = Esq.buildDType . runMaybeT $ do
  searchRequest <- Esq.findByIdM @SearchRequestSpecialZoneT $ toKey searchRequestId
  fetchFullSearchRequestSpecialZoneM searchRequest

findByMsgIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByMsgIdAndBapIdAndBppId txnId bapId merchantId = Esq.buildDType . runMaybeT $ do
  searchRequest <- Esq.findOneM $ do
    sReq <- from $ table @SearchRequestSpecialZoneT
    where_ $
      sReq ^. SearchRequestSpecialZoneMessageId ==. val txnId
        &&. sReq ^. SearchRequestSpecialZoneProviderId ==. val (toKey merchantId)
        &&. sReq ^. SearchRequestSpecialZoneBapId ==. val bapId
    pure sReq
  fetchFullSearchRequestSpecialZoneM searchRequest

-- internal queries for building domain types

fetchFullSearchRequestSpecialZoneM ::
  Transactionable m =>
  SearchRequestSpecialZoneT ->
  MaybeT (DTypeBuilder m) (SolidType FullSearchRequestSpecialZoneT)
fetchFullSearchRequestSpecialZoneM searchRequest@SearchRequestSpecialZoneT {..} = do
  fromLocation <- Esq.findByIdM @SearchReqLocationT fromLocationId
  toLocation <- Esq.findByIdM @SearchReqLocationT toLocationId
  pure $ extractSolidType @SearchRequestSpecialZone (searchRequest, fromLocation, toLocation)
