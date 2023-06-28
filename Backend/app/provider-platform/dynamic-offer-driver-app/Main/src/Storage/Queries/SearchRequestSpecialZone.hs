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

import Domain.Types.Location as DLocation
import Domain.Types.LocationMapping as DLocationMapping
import Domain.Types.Merchant
import Domain.Types.SearchRequestSpecialZone as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.SearchRequestSpecialZone

create :: SearchRequestSpecialZone -> SqlDB ()
create searchRequest = do
  let tablex = searchReqSPToSearchTableConverter searchRequest
  createSearchReqTable tablex

searchReqSPToSearchTableConverter :: SearchRequestSpecialZone -> SearchRequestSpecialZoneTable
searchReqSPToSearchTableConverter SearchRequestSpecialZone {..} = SearchRequestSpecialZoneTable {..}

createSearchReqTable :: SearchRequestSpecialZoneTable -> SqlDB ()
createSearchReqTable = Esq.create

findById :: (Transactionable m, MonadFlow m) => Id SearchRequestSpecialZone -> m (Maybe SearchRequestSpecialZone)
findById searchRequestId = do
  mbSearchReqTable <- getSearchRequestSpecialZoneTableById searchRequestId
  case mbSearchReqTable of
    Just searchRequestTable -> searchTableToSearchReqSpecialZoneConverter searchRequestTable
    Nothing -> return Nothing

searchTableToSearchReqSpecialZoneConverter :: (Transactionable m, MonadFlow m) => SearchRequestSpecialZoneTable -> m (Maybe SearchRequestSpecialZone)
searchTableToSearchReqSpecialZoneConverter SearchRequestSpecialZoneTable {..} = do
  locationMappings <- QLocationMapping.findByTagId id.getId
  let toLocation = map (.location) (filter (\locationMapping -> locationMapping.order /= 0) locationMappings)
  fromLocation <- sourceLocationFinder locationMappings & fromMaybeM (InternalError "from location is missing")
  return $
    Just
      SearchRequestSpecialZone
        { ..
        }

getSearchRequestSpecialZoneTableById ::
  (Transactionable m) =>
  Id SearchRequestSpecialZone ->
  m (Maybe SearchRequestSpecialZoneTable)
getSearchRequestSpecialZoneTableById searchRequestId = do
  findOne $ do
    searchRequestT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchRequestT ^. SearchRequestSpecialZoneId ==. val (getId searchRequestId)
    return searchRequestT

sourceLocationFinder :: [DLocationMapping.LocationMapping] -> Maybe Location
sourceLocationFinder locationMappings = do
  let orderZeroMappings = filter (\locationMapping -> locationMapping.order == 0) locationMappings
  if null orderZeroMappings
    then Nothing
    else do
      let source = head orderZeroMappings
      Just $ source.location

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

findTByMsgIdAndBapIdAndBppId :: Transactionable m => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZoneTable)
findTByMsgIdAndBapIdAndBppId txnId bapId merchantId = do
  findOne $ do
    searchT <- from $ table @SearchRequestSpecialZoneT
    where_ $
      searchT ^. SearchRequestSpecialZoneMessageId ==. val txnId
        &&. searchT ^. SearchRequestSpecialZoneProviderId ==. val (toKey merchantId)
        &&. searchT ^. SearchRequestSpecialZoneBapId ==. val bapId
    return searchT

findByMsgIdAndBapIdAndBppId :: (Transactionable m, MonadFlow m) => Text -> Text -> Id Merchant -> m (Maybe SearchRequestSpecialZone)
findByMsgIdAndBapIdAndBppId txnId bapId merchantId = do
  mbSearchReqTable <- findTByMsgIdAndBapIdAndBppId txnId bapId merchantId
  case mbSearchReqTable of
    Just searchRequestTable -> searchTableToSearchReqSpecialZoneConverter searchRequestTable
    Nothing -> return Nothing

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
