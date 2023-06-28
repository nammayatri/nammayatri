{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.SearchRequest where

import Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLocationMapping
import Domain.Types.SearchRequest as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.SearchRequest

create :: SearchRequest -> SqlDB ()
create searchRequest = do
  let tablex = searchReqToSearchTableConverter searchRequest
  createSearchReqTable tablex

createSearchReqTable :: SearchRequestTable -> SqlDB ()
createSearchReqTable = Esq.create

findById :: (Transactionable m, MonadFlow m) => Id SearchRequest -> m (Maybe SearchRequest)
findById searchRequestId = do
  mbSearchReqTable <- getSearchRequestTableById searchRequestId
  case mbSearchReqTable of
    Just searchRequestTable -> searchTableToSearchReqConverter searchRequestTable
    Nothing -> return Nothing

getSearchRequestTableById ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe SearchRequestTable)
getSearchRequestTableById searchRequestId = do
  findOne $ do
    searchRequestT <- from $ table @SearchRequestT
    where_ $
      searchRequestT ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequestT

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

searchReqToSearchTableConverter :: SearchRequest -> SearchRequestTable
searchReqToSearchTableConverter SearchRequest {..} = SearchRequestTable {..}

sourceLocationFinder :: [DLocationMapping.LocationMapping] -> Maybe Location
sourceLocationFinder locationMappings = do
  let orderZeroMappings = filter (\locationMapping -> locationMapping.order == 0) locationMappings
  if null orderZeroMappings
    then Nothing
    else do
      let source = head orderZeroMappings
      Just $ source.location

searchTableToSearchReqConverter :: (Transactionable m, MonadFlow m) => SearchRequestTable -> m (Maybe SearchRequest)
searchTableToSearchReqConverter SearchRequestTable {..} = do
  locationMappings <- QLocationMapping.findByTagId id.getId
  let toLocation = map (.location) (filter (\locationMapping -> locationMapping.order /= 0) locationMappings)
  fromLocation <- sourceLocationFinder locationMappings & fromMaybeM (InternalError "from location is missing")
  return $
    Just
      SearchRequest
        { ..
        }
