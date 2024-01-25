{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking where

import qualified Beckn.ACL.Update as ACL
import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.Booking as SRB
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Maps (LatLong)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Ride as QR
import Tools.Error

data StopReq = StopReq
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> m SRB.BookingAPIEntity
bookingStatus bookingId _ = do
  booking <- runInReplica (QRB.findById bookingId) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  logInfo $ "booking: test " <> show booking
  SRB.buildBookingAPIEntity booking booking.riderId

bookingList :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> m BookingListRes
bookingList (personId, _) mbLimit mbOffset mbOnlyActive mbBookingStatus = do
  rbList <- runInReplica $ QR.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus
  logInfo $ "rbList: test " <> show rbList
  BookingListRes <$> traverse (`SRB.buildBookingAPIEntity` personId) rbList

addStop :: (Id Person.Person, Id Merchant) -> Id SRB.Booking -> StopReq -> Flow APISuccess
addStop (_, merchantId) bookingId req = do
  processStop bookingId req merchantId False
  pure Success

editStop :: (Id Person.Person, Id Merchant) -> Id SRB.Booking -> StopReq -> Flow APISuccess
editStop (_, merchantId) bookingId req = do
  processStop bookingId req merchantId True
  pure Success

processStop :: Id SRB.Booking -> StopReq -> Id Merchant -> Bool -> Flow ()
processStop bookingId loc merchantId isEdit = do
  booking <- runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  validateStopReq booking isEdit
  location <- buildLocation loc
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.merchantId) (Just booking.merchantOperatingCityId)
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking (Just location)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let dUpdateReq =
        if isEdit
          then
            ACL.EditStopBuildReq
              { bppId = booking.providerId,
                bppUrl = booking.providerUrl,
                transactionId = booking.transactionId,
                stops = [mkDomainLocation location],
                ..
              }
          else
            ACL.AddStopBuildReq
              { bppId = booking.providerId,
                bppUrl = booking.providerUrl,
                transactionId = booking.transactionId,
                stops = [mkDomainLocation location],
                ..
              }
  becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq

validateStopReq :: (MonadFlow m) => SRB.Booking -> Bool -> m ()
validateStopReq booking isEdit = do
  unless (booking.status `elem` SRB.activeBookingStatus) $ throwError (RideInvalidStatus $ "Cannot edit/add stop in this booking " <> booking.id.getId)
  case booking.bookingDetails of
    SRB.OneWayDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in static offer on demand rides"
    SRB.RentalDetails SRB.RentalBookingDetails {..} ->
      if isEdit
        then unless (isJust stopLocation) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> booking.id.getId)
        else unless (isNothing stopLocation) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> booking.id.getId)
    SRB.DriverOfferDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in dynamic offer on demand rides"
    SRB.OneWaySpecialZoneDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in special zone rides"

buildLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => StopReq -> m Location
buildLocation req = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Location
      { lat = req.gps.lat,
        lon = req.gps.lon,
        address = req.address,
        createdAt = now,
        updatedAt = now,
        ..
      }

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Location -> Text -> Bool -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> m DLM.LocationMapping
buildLocationMapping locationId entityId isEdit merchantId merchantOperatingCityId = do
  id <- generateGUID
  now <- getCurrentTime
  prevOrder <- QLM.maxOrderByEntity entityId
  when isEdit $ QLM.updatePastMappingVersions entityId prevOrder
  let version = "LATEST"
      tag = DLM.BOOKING
  return $
    DLM.LocationMapping
      { order = if isEdit then prevOrder else prevOrder + 1,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkDomainLocation :: Location -> Common.Location
mkDomainLocation Location {..} =
  Common.Location
    { gps = Common.Gps {..},
      address =
        Common.Address
          { locality = address.area,
            area_code = address.areaCode,
            state = address.state,
            country = address.country,
            building = address.building,
            street = address.street,
            city = address.city,
            ward = address.ward,
            door = address.door
          }
    }
