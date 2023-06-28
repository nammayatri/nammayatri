{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.SearchRequest where

import Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLocationMapping
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Queries.LocationMapping as QLocationMapping
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

findByPersonId :: (Transactionable m, MonadFlow m) => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId personId searchRequestId = do
  mbSearchReqTable <- findSearchRequestTableByPersonId personId searchRequestId
  case mbSearchReqTable of
    Just searchRequestTable -> searchTableToSearchReqConverter searchRequestTable
    Nothing -> return Nothing

findSearchRequestTableByPersonId :: Transactionable m => Id Person -> Id SearchRequest -> m (Maybe SearchRequestTable)
findSearchRequestTableByPersonId personId searchRequestId = do
  findOne $ do
    searchRequest <- from $ table @SearchRequestT
    where_ $
      searchRequest ^. SearchRequestRiderId ==. val (toKey personId)
        &&. searchRequest ^. SearchRequestId ==. val (getId searchRequestId)
    return searchRequest

findAllByPerson :: (Transactionable m, MonadFlow m) => Id Person -> m [SearchRequest]
findAllByPerson perId = do
  searchReqTables <- findAllTableByPerson perId
  catMaybes <$> mapM searchTableToSearchReqConverter searchReqTables

findAllTableByPerson :: Transactionable m => Id Person -> m [SearchRequestTable]
findAllTableByPerson perId = findAll $ do
  searchRequest <- from $ table @SearchRequestT
  where_ $
    searchRequest ^. SearchRequestRiderId ==. val (toKey perId)
  return searchRequest

updateCustomerExtraFee :: Id SearchRequest -> Maybe Money -> SqlDB ()
updateCustomerExtraFee searchReqId customerExtraFee = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ SearchRequestCustomerExtraFee =. val customerExtraFee
      ]
    where_ $ tbl ^. SearchRequestId ==. val (getId searchReqId)

getSearchTableBySearchId ::
  (Transactionable m) =>
  Id SearchRequest ->
  m (Maybe SearchRequestTable)
getSearchTableBySearchId searchId = do
  findOne $ do
    bookingT <- from $ table @SearchRequestT
    where_ $
      bookingT ^. SearchRequestId ==. val (getId searchId)
    return bookingT

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
