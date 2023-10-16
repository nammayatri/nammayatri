{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Location where

import qualified Domain.Types.Booking.BookingLocation as DBBL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSSL
import EulerHS.Prelude (whenNothingM_)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.SearchRequest.SearchReqLocation as QSRL

buildPickUpLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m DLM.LocationMapping
buildPickUpLocationMapping locationId entityId tag = do
  id <- generateGUID
  let order = 0
  QLM.updatePastMappingVersions entityId order
  let version = "LATEST"
  return DLM.LocationMapping {..}

buildDropLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m DLM.LocationMapping
buildDropLocationMapping locationId entityId tag = do
  id <- generateGUID
  noOfEntries <- QLM.countOrders entityId
  let order = if noOfEntries == 0 then 1 else noOfEntries
  QLM.updatePastMappingVersions entityId order
  let version = "LATEST"
  return DLM.LocationMapping {..}

createPickUpLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m ()
createPickUpLocationMapping locationId entityId tag = do
  fromLocationMap <- buildPickUpLocationMapping locationId entityId tag
  void $ QLM.create fromLocationMap

createDropLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m ()
createDropLocationMapping locationId entityId tag = do
  mbToLocationMap <- Just <$> buildDropLocationMapping locationId entityId tag
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap

buildBookingLocation :: MonadFlow m => DBBL.BookingLocation -> m DL.Location
buildBookingLocation DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        ..
      }

buildSearchReqLocation :: MonadFlow m => DSSL.SearchReqLocation -> m DL.Location
buildSearchReqLocation DSSL.SearchReqLocation {..} = do
  return $
    DL.Location
      { id = cast id,
        ..
      }

upsertFromLocationAndMappingForOldData :: MonadFlow m => Maybe Text -> Text -> DLM.LocationMappingTags -> m DL.Location
upsertFromLocationAndMappingForOldData locationId entityId tag = do
  pickupLoc <- case tag of
    DLM.BOOKING -> do
      loc <- QBBL.findById `mapM` (Id <$> locationId) >>= fromMaybeM (InternalError "From Location Id Not Found in Booking Table")
      maybe (throwError $ InternalError ("From Location Not Found in Booking Location Table for BookingId : " <> entityId)) buildBookingLocation loc
    DLM.SEARCH_REQUEST -> do
      loc <- QSRL.findById `mapM` (Id <$> locationId) >>= fromMaybeM (InternalError "From Location Id Not Found in Search Request Table")
      maybe (throwError $ InternalError ("From Location Not Found in Search Request Location Table for SearchRequestId : " <> entityId)) buildSearchReqLocation loc
    DLM.RIDE -> do
      throwError $ InternalError "No backfilling required for ride" -- this case will never come
  fromLocationMapping <- buildPickUpLocationMapping pickupLoc.id entityId tag
  void $ QL.create pickupLoc >> QLM.create fromLocationMapping
  return pickupLoc

upsertToLocationAndMappingForOldData :: MonadFlow m => Maybe Text -> Text -> DLM.LocationMappingTags -> m (Maybe DL.Location)
upsertToLocationAndMappingForOldData toLocationId entityId tag = do
  case tag of
    DLM.BOOKING -> do
      toLocation <- maybe (pure Nothing) (QBBL.findById . Id) toLocationId >>= fromMaybeM (InternalError "toLocation is null for one way booking")
      dropLoc <- buildBookingLocation toLocation
      toLocationMapping <- buildDropLocationMapping dropLoc.id entityId tag
      void $ QL.create dropLoc >> QLM.create toLocationMapping
      return $ Just dropLoc
    DLM.SEARCH_REQUEST -> do
      tl <- maybe (pure Nothing) (QSRL.findById . Id) toLocationId
      dropLocation <- maybe (pure Nothing) (fmap Just . buildSearchReqLocation) tl
      whenJust dropLocation $ \dropLoc -> do
        toLocationMapping <- buildDropLocationMapping dropLoc.id entityId DLM.SEARCH_REQUEST
        void $ QL.create dropLoc >> QLM.create toLocationMapping
      return dropLocation
    DLM.RIDE -> do
      throwError $ InternalError "No backfilling required for ride" -- this case will never come

createLocation :: MonadFlow m => DL.Location -> m ()
createLocation location = do
  whenNothingM_ (QL.findById location.id) $ do QL.create location
