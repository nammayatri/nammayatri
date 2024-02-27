{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Domain.Action.Beckn.Update where

import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import Data.Maybe
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
-- import qualified Domain.Types.Estimate as DEst
-- import qualified Domain.Types.EstimateRevised as DER

-- import qualified Domain.Types.EstimateRevised as DER
-- import Domain.Types.FareParameters
-- import qualified Domain.Types.FarePolicy as DFP
-- import qualified Domain.Types.FareProduct as DFareProduct
-- import SharedLogic.FareCalculator

-- import qualified Storage.Queries.Estimate as QEst

import qualified Domain.Types.DriverLocation as DDL
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.EstimateCalculation as SHEC
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.FarePolicy
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.EstimateRevised as QER
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QuoteRevised as QQR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Notifications as Notify

data DUpdateReq
  = PaymentCompletedReq
      { bookingId :: Id DBooking.Booking,
        rideId :: Id DRide.Ride,
        paymentStatus :: PaymentStatus,
        paymentMethodInfo :: DMPM.PaymentMethodInfo
      }
  | EditLocationReq
      { bookingId :: Id DBooking.Booking,
        rideId :: Id DRide.Ride,
        origin :: Maybe Common.Location,
        destination :: Maybe Common.Location,
        transactionId :: Text
      }
  | AddStopReq
      { bookingId :: Id DBooking.Booking,
        stops :: [Common.Location]
      }
  | EditStopReq
      { bookingId :: Id DBooking.Booking,
        stops :: [Common.Location]
      }
  | ConfirmEstimateReq
      { bookingId :: Id DBooking.Booking,
        rideId :: Id DRide.Ride,
        -- transactionId :: Text,
        confirmEstimateStatus :: Bool
      }

data PaymentStatus = PAID | NOT_PAID

data DistanceAndDuration = DistanceAndDuration
  { distance :: Meters,
    duration :: Seconds
  }

getDistanceAndDuration :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> LatLong -> Flow DistanceAndDuration
getDistanceAndDuration merchantId merchantOpCityId fromLocation toLocation = do
  response <-
    Maps.getDistance merchantId merchantOpCityId $
      Maps.GetDistanceReq
        { origin = fromLocation,
          destination = toLocation,
          travelMode = Just Maps.CAR
        }
  return DistanceAndDuration {distance = response.distance, duration = response.duration}

handler :: DUpdateReq -> Flow ()
handler req@PaymentCompletedReq {} = do
  unless (req.paymentMethodInfo.paymentType == DMPM.ON_FULFILLMENT) $
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
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for booking " <> bookingId.getId)
    Just loc -> processStop booking loc False
handler EditStopReq {..} = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for booking " <> bookingId.getId)
    Just loc -> processStop booking loc True
handler EditLocationReq {..} = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  whenJust origin $ \pickup -> do
    startLocation <- buildLocation pickup
    QL.create startLocation
    pickupMapForBooking <- SLM.buildPickUpLocationMapping startLocation.id bookingId.getId DLM.BOOKING (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForBooking
    pickupMapForRide <- SLM.buildPickUpLocationMapping startLocation.id rideId.getId DLM.RIDE (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForRide
    let entityData = Notify.EditLocationReq {..}
    Notify.notifyPickupOrDropLocationChange person.merchantOperatingCityId person.id person.deviceToken entityData
  whenJust destination $ \dropOff -> do
    endLocation <- buildLocation dropOff
    QL.create endLocation
    let updatedDropoffLocationKey = makeUpdatedDropoffLocationKey rideId
    Redis.setExp updatedDropoffLocationKey endLocation 600 -- 10 miuntes
    merchantOpCityId <- pure $ ride.merchantOperatingCityId
    fromLocationLatLong <- pure $ locationToLatLon ride.fromLocation
    currentLocation <- do
      driverLocation <- LF.driversLocation [ride.driverId]
      listToMaybe (driverLocation) & fromMaybeM LocationNotFound
    currentLocationLatLong <- pure $ driverLocationToLatLong currentLocation
    updatedToLocationLatLong <- pure $ gpsToLatLon dropOff.gps
    originToCurrentLocation <- getDistanceAndDuration person.merchantId merchantOpCityId fromLocationLatLong currentLocationLatLong
    currentLocationToNewDestination <- getDistanceAndDuration person.merchantId merchantOpCityId currentLocationLatLong updatedToLocationLatLong
    totalDistance <- pure $ originToCurrentLocation.distance + currentLocationToNewDestination.distance
    totalDuration <- pure $ originToCurrentLocation.duration + currentLocationToNewDestination.duration
    searchReq <- QSR.findByTransactionId transactionId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId)
    booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
    allFarePoliciesProduct <- getAllFarePoliciesProduct person.merchantId merchantOpCityId fromLocationLatLong (Just updatedToLocationLatLong) booking.tripCategory
    farePolicy <- getFarePolicy merchantOpCityId booking.tripCategory booking.vehicleVariant (Just allFarePoliciesProduct.area)
    estimateRevised <- SHEC.buildEstimateRevised searchReq.id booking.startTime searchReq.isScheduled (Just totalDistance) allFarePoliciesProduct.specialLocationTag searchReq.customerCancellationDues False farePolicy
    quoteRevised <- SHEC.buildQuoteRevised searchReq person.merchantId booking.startTime searchReq.isScheduled (Just totalDistance) (Just totalDuration) allFarePoliciesProduct.specialLocationTag searchReq.customerCancellationDues False farePolicy
    QER.create estimateRevised
    QQR.create quoteRevised
    let entityData = Notify.EditLocationReq {..}
    merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
    triggerEstimateRevisedEvent EstimateRevisedEventData {estimateRevised = estimateRevised, merchantId = person.merchantId}
    Notify.notifyPickupOrDropLocationChange person.merchantOperatingCityId person.id person.deviceToken entityData --
    CallBAP.sendUpdatedEstimateToBAP booking ride estimateRevised quoteRevised booking.startTime fromLocationLatLong (Just updatedToLocationLatLong) merchant allFarePoliciesProduct.specialLocationTag
  where
    gpsToLatLon Common.Gps {..} = LatLong {..}
    locationToLatLon DL.Location {..} = LatLong {..}
    driverLocationToLatLong DDL.DriverLocation {..} = LatLong {..}
handler req@ConfirmEstimateReq {} = do
  confirmEstimateStatus <- pure $ req.confirmEstimateStatus
  let updatedDropoffLocationKey = makeUpdatedDropoffLocationKey req.rideId
  endLoaction :: DL.Location <- Redis.safeGet updatedDropoffLocationKey >>= fromMaybeM (InternalError "endloc not found")
  case confirmEstimateStatus of
    True -> do
      ride <- runInReplica $ QRide.findById req.rideId >>= fromMaybeM (RideNotFound req.rideId.getId)
      person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      dropoffMapForBooking <- SLM.buildDropLocationMapping endLoaction.id req.bookingId.getId DLM.BOOKING (Just person.merchantId) (Just person.merchantOperatingCityId)
      QLM.create dropoffMapForBooking
      dropoffMapForRide <- SLM.buildDropLocationMapping endLoaction.id req.rideId.getId DLM.RIDE (Just person.merchantId) (Just person.merchantOperatingCityId)
      QLM.create dropoffMapForRide
    --booking table
    False -> do
      logDebug $ "make no changes"

-- handler _ = throwError (InvalidRequest "Not Implemented")

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
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, city, state, area_code, country]
  if null strictFields
    then Nothing
    else Just $ T.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (T.null . T.strip)

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> Bool -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> m DLM.LocationMapping
buildLocationMapping locationId entityId isEdit merchantId merchantOperatingCityId = do
  id <- generateGUID
  now <- getCurrentTime
  prevOrder <- QLM.maxOrderByEntity entityId
  when isEdit $ QLM.updatePastMappingVersions entityId prevOrder
  let version = QLM.latestTag
      tag = DLM.BOOKING
  return $
    DLM.LocationMapping
      { order = if isEdit then prevOrder else prevOrder + 1,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkLocationAPIEntity :: Common.Location -> DL.LocationAPIEntity
mkLocationAPIEntity location =
  DL.LocationAPIEntity
    { lat = location.gps.lat,
      lon = location.gps.lon,
      street = location.address.street,
      city = location.address.city,
      state = location.address.state,
      country = location.address.country,
      building = location.address.building,
      areaCode = location.address.area_code,
      area = location.address.ward,
      fullAddress = mkFullAddress location.address
    }

processStop :: DBooking.Booking -> Common.Location -> Bool -> Flow ()
processStop booking loc isEdit = do
  validateStopReq booking isEdit
  location <- buildLocation loc
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.providerId) (Just booking.merchantOperatingCityId)
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking.id (Just location.id.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  whenJust mbRide $ \ride -> do
    person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    let entityData = Notify.StopReq {bookingId = booking.id, stop = Just (mkLocationAPIEntity loc), ..}
    when (ride.status == DRide.INPROGRESS) $ Notify.notifyStopModification person.merchantOperatingCityId person.id person.deviceToken entityData

validateStopReq :: DBooking.Booking -> Bool -> Flow ()
validateStopReq booking isEdit = do
  unless (booking.status `elem` [DBooking.NEW, DBooking.TRIP_ASSIGNED]) $ throwError $ BookingInvalidStatus ("Cannot add stop in this booking " <> booking.id.getId) -- check for rental?
  if isEdit
    then unless (isJust booking.stopLocationId) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> booking.id.getId) -- should we throw error or just allow?
    else unless (isNothing booking.stopLocationId) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> booking.id.getId)

makeUpdatedDropoffLocationKey :: Id DRide.Ride -> Text
makeUpdatedDropoffLocationKey id = "makeUpdatedDropoffLocationKey:RideId-" <> id.getId
