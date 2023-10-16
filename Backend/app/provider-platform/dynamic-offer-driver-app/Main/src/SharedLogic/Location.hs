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

createPickupLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m ()
createPickupLocationMapping locationId entityId tag = do
  locationMap <- buildPickUpLocationMapping locationId entityId tag
  QLM.create locationMap

createDropLocationMapping :: MonadFlow m => Id DL.Location -> Text -> DLM.LocationMappingTags -> m ()
createDropLocationMapping locationId entityId tag = do
  locationMap <- buildDropLocationMapping locationId entityId tag
  QLM.create locationMap

backfillLocationAndLocationMapping :: MonadFlow m => Maybe Text -> Maybe Text -> Text -> DLM.LocationMappingTags -> m (DL.Location, DL.Location)
backfillLocationAndLocationMapping fromLocationId toLocationId entityId tag = do
  pickupLoc <- upsertLocationForOldData fromLocationId entityId tag
  pickupLocMapping <- buildPickUpLocationMapping pickupLoc.id entityId tag
  QLM.create pickupLocMapping

  dropLoc <- upsertLocationForOldData toLocationId entityId tag
  dropLocMapping <- buildDropLocationMapping dropLoc.id entityId tag
  QLM.create dropLocMapping
  return (pickupLoc, dropLoc)

mkLocationAddress :: DBBL.LocationAddress -> DL.LocationAddress
mkLocationAddress DBBL.LocationAddress {..} =
  DL.LocationAddress
    { fullAddress = Nothing,
      ..
    }

buildBookingLocation :: MonadFlow m => DBBL.BookingLocation -> m DL.Location
buildBookingLocation DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        address = mkLocationAddress address,
        ..
      }

buildSearchReqLocation :: MonadFlow m => DSSL.SearchReqLocation -> m DL.Location
buildSearchReqLocation DSSL.SearchReqLocation {..} =
  return $
    DL.Location
      { id = cast id,
        address =
          DL.LocationAddress
            { fullAddress = full_address,
              ..
            },
        ..
      }

upsertLocationForOldData :: MonadFlow m => Maybe Text -> Text -> DLM.LocationMappingTags -> m DL.Location
upsertLocationForOldData locationId entityId tag = do
  location <- case tag of
    DLM.BOOKING -> do
      loc <- QBBL.findById `mapM` (Id <$> locationId) >>= fromMaybeM (InternalError "Location Id Not Found in Booking Location Table")
      maybe (throwError $ InternalError ("Location Not Found in Booking Location Table for BookingId : " <> entityId)) buildBookingLocation loc
    DLM.SEARCH_REQUEST -> do
      loc <- QSRL.findById `mapM` (Id <$> locationId) >>= fromMaybeM (InternalError "LocationId Not Found in Search Request Location Table")
      maybe (throwError $ InternalError $ "Location not found in SearchRequest for Search Request Id:" <> show entityId) buildSearchReqLocation loc
    DLM.RIDE -> do
      throwError $ InternalError "No backfilling required for ride table" -- this case will never come
  void $ QL.create location
  return location

createLocation :: MonadFlow m => DL.Location -> m ()
createLocation location = do
  whenNothingM_ (QL.findById location.id) $ do QL.create location
