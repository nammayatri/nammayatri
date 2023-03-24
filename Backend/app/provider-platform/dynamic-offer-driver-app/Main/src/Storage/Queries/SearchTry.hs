{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchTry where

import Domain.Types.SearchTry as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.SearchRequest.SearchReqLocation
import Storage.Tabular.SearchTry

create :: SearchTry -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

findById :: Transactionable m => Id SearchTry -> m (Maybe SearchTry)
findById searchRequestId = buildDType $
  fmap (fmap $ extractSolidType @Domain.SearchTry) $
    Esq.findOne' $ do
      (sReq :& sFromLoc :& sToLoc) <-
        from
          ( table @SearchTryT
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& loc1) -> s ^. SearchTryFromLocationId ==. loc1 ^. SearchReqLocationTId)
              `innerJoin` table @SearchReqLocationT `Esq.on` (\(s :& _ :& loc2) -> s ^. SearchTryToLocationId ==. loc2 ^. SearchReqLocationTId)
          )
      where_ $ sReq ^. SearchTryTId ==. val (toKey searchRequestId)
      pure (sReq, sFromLoc, sToLoc)

updateStatus ::
  Id SearchTry ->
  SearchTryStatus ->
  SqlDB ()
updateStatus searchId status_ = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchTryUpdatedAt =. val now,
        SearchTryStatus =. val status_
      ]
    where_ $ tbl ^. SearchTryTId ==. val (toKey searchId)

getRequestIdfromTransactionId ::
  (Transactionable m) =>
  Id SearchTry ->
  m (Maybe (Id SearchTry))
getRequestIdfromTransactionId tId = do
  findOne $ do
    searchT <- from $ table @SearchTryT
    where_ $
      searchT ^. SearchTryTransactionId ==. val (getId tId)
    return $ searchT ^. SearchTryTId

getSearchRequestStatusOrValidTill ::
  (Transactionable m) =>
  Id SearchTry ->
  m (Maybe (UTCTime, SearchTryStatus))
getSearchRequestStatusOrValidTill searchRequestId = do
  findOne $ do
    searchT <- from $ table @SearchTryT
    where_ $
      searchT ^. SearchTryTId ==. val (toKey searchRequestId)
    return (searchT ^. SearchTryValidTill, searchT ^. SearchTryStatus)

findActiveByTransactionId ::
  (Transactionable m) =>
  Text ->
  m (Maybe (Id SearchTry))
findActiveByTransactionId transactionId = do
  findOne $ do
    searchT <- from $ table @SearchTryT
    where_ $
      searchT ^. SearchTryTransactionId ==. val transactionId
        &&. searchT ^. SearchTryStatus ==. val Domain.ACTIVE
    return $ searchT ^. SearchTryTId
