{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Update where

import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.Beam.Functions as B
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

data DUpdateReq
  = PaymentCompletedReq
      { bookingId :: Id DBooking.Booking,
        rideId :: Id DRide.Ride,
        paymentStatus :: PaymentStatus,
        paymentMethodInfo :: DMPM.PaymentMethodInfo
      }
  | AddStopReq
      { bookingId :: Id DBooking.Booking,
        rideId :: Id DRide.Ride,
        stops :: [Common.Location]
      }
  | EditStopReq
      { bookingId :: Id DBooking.Booking,
        rideId :: Id DRide.Ride,
        stops :: [Common.Location]
      }

data PaymentStatus = PAID | NOT_PAID

handler :: DUpdateReq -> Flow ()
handler req@PaymentCompletedReq {} = do
  unless (req.paymentMethodInfo.paymentType == DMPM.POSTPAID) $
    throwError $ InvalidRequest "Payment completed update available only for POSTPAID payments."
  unless (req.paymentMethodInfo.collectedBy == DMPM.BAP) $
    throwError $ InvalidRequest "Payment completed update available only when BAP collect payment."
  when (req.paymentMethodInfo.paymentInstrument == DMPM.Cash) $
    throwError $ InvalidRequest "Payment completed update not available for cash"
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  paymentMethodId <- booking.paymentMethodId & fromMaybeM (InvalidRequest "Payment method not specified for this booking.")
  paymentMethod <-
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod
  unless (req.paymentMethodInfo == paymentMethodInfo) $
    throwError (InvalidRequest $ "Invalid payment method info for this booking, should be: " <> show paymentMethodInfo <> ".")
  ride <-
    QRide.findById req.rideId
      >>= fromMaybeM (RideNotFound booking.id.getId)
  unless (ride.status == DRide.COMPLETED) $
    throwError $ RideInvalidStatus "Ride is not completed yet."
  logTagInfo "Payment completed : " ("bookingId " <> req.bookingId.getId <> ", rideId " <> req.rideId.getId)
handler AddStopReq {..} = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  validateStopReq ride False
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for ride " <> rideId.getId)
    Just loc -> processStop rideId loc False
handler EditStopReq {..} = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  validateStopReq ride True
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for ride " <> rideId.getId)
    Just loc -> processStop rideId loc True

processStop :: Id DRide.Ride -> Common.Location -> Bool -> Flow ()
processStop rideId loc isEdit = do
  location <- buildLocation loc
  locationMapping <- buildLocationMapping location.id rideId.getId isEdit
  QL.create location
  QLM.create locationMapping
  QRide.updateNextStop rideId (Just location.id.getId)
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  let entityData = Notify.StopReq {..}
  Notify.notifyStopModification person.merchantOperatingCityId person.id person.deviceToken entityData

validateStopReq :: DRide.Ride -> Bool -> Flow ()
validateStopReq ride isEdit = do
  unless (ride.status `elem` [DRide.INPROGRESS, DRide.NEW]) $ throwError $ RideInvalidStatus ("Cannot add stop in this ride " <> ride.id.getId)
  if isEdit
    then unless (isJust ride.nextStopLocId) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> ride.id.getId) -- should we throw error or just allow?
    else unless (isNothing ride.nextStopLocId) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> ride.id.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.bookingType == DBooking.RentalBooking) $ throwError (RideInvalidStatus $ "Cannot add/edit stop in non rental rides " <> ride.id.getId)

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> Bool -> m DLM.LocationMapping
buildLocationMapping locationId entityId isEdit = do
  id <- generateGUID
  prevOrder <- QLM.maxOrderByEntity entityId
  when isEdit $ QLM.updatePastMappingVersions entityId prevOrder
  let version = "LATEST"
      tag = DLM.RIDE
  return $
    DLM.LocationMapping
      { order = if isEdit then prevOrder else prevOrder + 1,
        ..
      }

buildLocation :: MonadFlow m => Common.Location -> m DL.Location
buildLocation location = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DL.Location
      { id = guid,
        createdAt = now,
        updatedAt = now,
        lat = location.gps.lat,
        lon = location.gps.lon,
        address =
          DL.LocationAddress
            { street = location.address.street,
              door = location.address.door,
              city = location.address.city,
              state = location.address.state,
              country = location.address.country,
              building = location.address.building,
              areaCode = location.address.area_code,
              area = location.address.locality,
              fullAddress = mkFullAddress location.address
            }
      }

mkFullAddress :: Common.Address -> Maybe Text
mkFullAddress Common.Address {..} = do
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, locality, city, state, area_code, country]
  if null strictFields
    then Nothing
    else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.strip)
