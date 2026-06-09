{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Update where

import qualified API.Types.UI.EditBooking as EditBooking
import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified BecknV2.OnDemand.Enums as Enums
import Data.List (last)
import Data.List.Split (chunksOf)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.Internal.ViolationDetection as VID
import qualified Domain.Action.UI.EditBooking as EditBooking
import qualified Domain.Action.UI.Location as DL
import Domain.Action.UI.Ride.EndRide.Internal
import qualified Domain.Types as DVST
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.OnUpdate
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideRoute as RR
import Domain.Types.TransporterConfig (TransporterConfig)
import Domain.Types.Trip (isRideOtpTrip)
import qualified Domain.Types.Trip as DTC
import Environment
import EulerHS.Prelude hiding (drop, id, state)
import Kernel.Beam.Functions as B
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.LocationUpdates.Internal
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.CallBAP
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.FareCalculator
import qualified SharedLogic.FareCalculator as FC
import SharedLogic.FarePolicy
import qualified SharedLogic.LocationMapping as SLM
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import SharedLogic.Ride
import qualified SharedLogic.Type as SLT
import Storage.Beam.Toll ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DomainDiscountConfig as CQDDC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.BookingUpdateRequestExtra as QBURExtra
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as SQSR
import Toll.SharedLogic.TollsDetector
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

data DUpdateReq
  = UPaymentCompletedReq PaymentCompletedReq
  | UEditLocationReq EditLocationReq
  | UAddStopReq AddStopReq
  | UEditStopReq EditStopReq
  | UEditRideStopsReq EditRideStopsReq
  | UChangeServiceTierReq ChangeServiceTierReq
  | UAddBaggageReq AddBaggageReq

data PaymentCompletedReq = PaymentCompletedReq
  { bookingId :: Id DBooking.Booking,
    rideId :: Id DRide.Ride,
    paymentStatus :: PaymentStatus,
    paymentMethodInfo :: DMPM.PaymentMethodInfo
  }

data EditLocationReq = EditLocationReq
  { bookingId :: Id DBooking.Booking,
    rideId :: Id DRide.Ride,
    origin' :: Maybe DL.Location',
    destination' :: Maybe DL.Location',
    status :: Enums.OrderStatus,
    bapBookingUpdateRequestId :: Text,
    transactionId :: Text
  }

data AddStopReq = AddStopReq
  { bookingId :: Id DBooking.Booking,
    stops' :: [DL.Location']
  }

data EditStopReq = EditStopReq
  { bookingId :: Id DBooking.Booking,
    stops' :: [DL.Location']
  }

data EditRideStopsReq = EditRideStopsReq
  { bookingId :: Id DBooking.Booking,
    rideId :: Id DRide.Ride,
    stops' :: [DL.Location'],
    status :: Enums.OrderStatus,
    bapBookingUpdateRequestId :: Text,
    transactionId :: Text,
    preservedPrefixStops :: Maybe Int
  }

data ChangeServiceTierReq = ChangeServiceTierReq
  { bookingId :: Id DBooking.Booking,
    newVehicleServiceTier :: DVST.ServiceTierType,
    bppQuoteId :: Text
  }

data AddBaggageReq = AddBaggageReq
  { bookingId :: Id DBooking.Booking,
    numberOfLuggages :: Int
  }

mkLocation :: Id DMOC.MerchantOperatingCity -> DL.Location' -> DL.Location
mkLocation merchantOperatingCityId DL.Location' {..} = DL.Location {merchantOperatingCityId = Just merchantOperatingCityId, ..}

getBookingId :: DUpdateReq -> Id DBooking.Booking
getBookingId (UPaymentCompletedReq req) = req.bookingId
getBookingId (UEditLocationReq req) = req.bookingId
getBookingId (UAddStopReq req) = req.bookingId
getBookingId (UEditStopReq req) = req.bookingId
getBookingId (UEditRideStopsReq req) = req.bookingId
getBookingId (UChangeServiceTierReq req) = req.bookingId
getBookingId (UAddBaggageReq req) = req.bookingId

data PaymentStatus = PAID | NOT_PAID

handler :: DUpdateReq -> Flow ()
handler (UPaymentCompletedReq req@PaymentCompletedReq {}) = do
  unless (req.paymentMethodInfo.paymentType == DMPM.ON_FULFILLMENT) $
    throwError $ InvalidRequest "Payment completed update available only for ON_FULFILLMENT payments."
  unless (req.paymentMethodInfo.collectedBy == DMPM.BAP) $
    throwError $ InvalidRequest "Payment completed update available only when BAP collect payment."
  when (req.paymentMethodInfo.paymentInstrument `elem` [DMPM.Cash, DMPM.BoothOnline]) $
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
    throwError $ RideInvalidStatus ("Ride is not completed yet." <> Text.pack (show ride.status))
  logTagInfo "Payment completed : " ("bookingId " <> req.bookingId.getId <> ", rideId " <> req.rideId.getId)
handler (UAddStopReq AddStopReq {..}) = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  let stops = mkLocation booking.merchantOperatingCityId <$> stops'
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for booking " <> bookingId.getId)
    Just loc -> processStop booking loc False
handler (UEditStopReq EditStopReq {..}) = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  let stops = mkLocation booking.merchantOperatingCityId <$> stops'
  case listToMaybe stops of
    Nothing -> throwError (InvalidRequest $ "No stop information received from rider side for booking " <> bookingId.getId)
    Just loc -> processStop booking loc True
handler (UEditRideStopsReq EditRideStopsReq {..}) = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  when (ride.status == DRide.COMPLETED || ride.status == DRide.CANCELLED) $
    throwError $ RideInvalidStatus ("Can't edit stops on terminal ride: " <> Text.pack (show ride.status))
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  handleStopsModification booking ride rideId status stops' bapBookingUpdateRequestId preservedPrefixStops person
handler (UChangeServiceTierReq ChangeServiceTierReq {..}) = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DBooking.NEW) $
    throwError $ ChangeServiceTierInvalidBookingStatus (show booking.status)
  unless (isRideOtpTrip booking.tripCategory) $
    throwError ChangeServiceTierNotSupported
  when (newVehicleServiceTier == booking.vehicleServiceTier) $
    throwError ChangeServiceTierSameTier

  -- Load the quote from the original search to get pre-calculated fare
  quote <- QQuote.findById (Id bppQuoteId) >>= fromMaybeM (ChangeServiceTierQuoteNotFound bppQuoteId)
  unless (quote.vehicleServiceTier == newVehicleServiceTier) $
    throwError ChangeServiceTierQuoteTierMismatch

  -- NOTE: Ignoring quote expiry for now. The booking is already confirmed,
  -- and the quote's fare was valid at search time. If we need expiry checks
  -- later, add: unless (quote.validTill > now) $ throwError QuoteExpired

  -- Use the quote's pre-calculated fare params directly
  let newEstimatedFare = quote.estimatedFare
  let newFareParams = quote.fareParams

  -- Look up VehicleServiceTier config for AC/seating info
  mbVehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow newVehicleServiceTier booking.merchantOperatingCityId booking.configInExperimentVersions (booking.area >>= SL.pickupSpecialZoneIdFromArea)

  -- Persist new fare params and update booking
  QFP.create newFareParams
  QRB.updateVehicleServiceTierAndFare booking.id newVehicleServiceTier newFareParams.id newEstimatedFare quote.vehicleServiceTierName (getId quote.id) (mbVehicleServiceTierItem >>= (.airConditionedThreshold)) ((.seatingCapacity) =<< mbVehicleServiceTierItem) (mbVehicleServiceTierItem >>= (.isAirConditioned))

  -- NOTE: We skip BookingUpdateRequest for now. The confirm is a single atomic
  -- operation with no driver in the loop (RideOTP: status=NEW, no driver assigned).
  -- Introduce BookingUpdateRequest when extending to Static flow where a driver IS
  -- assigned and may need to accept/reject the tier change.

  -- Send on_update to BAP to confirm the change
  sendChangeServiceTierUpdateToBAP booking newVehicleServiceTier newEstimatedFare bppQuoteId
handler (UAddBaggageReq AddBaggageReq {..}) = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DBooking.NEW) $
    throwError $ AddBaggageInvalidBookingStatus (show booking.status)
  unless (isRideOtpTrip booking.tripCategory) $
    throwError AddBaggageNotSupported

  -- BPP-side cap check (defense in depth; BAP already validates against riderConfig)
  transporterCfg <-
    SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  whenJust transporterCfg.maxNumberOfLuggages $ \maxN ->
    when (numberOfLuggages > maxN) $ throwError (AddBaggageExceedsMax maxN)

  -- Recover original FullFarePolicy (cached at confirm-time via cacheFarePolicyByQuoteId, long TTL)
  fullFarePolicy <-
    getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
      >>= fromMaybeM (AddBaggageFarePolicyNotFound booking.quoteId)

  -- Mirror search-time nightShiftOverlapChecking, derived from tripCategory's pricing policy
  let nightShiftOverlapChecking = case DTC.tripCategoryToPricingPolicy booking.tripCategory of
        DTC.EstimateBased nsoc -> nsoc
        DTC.QuoteBased nsoc -> nsoc

  let origFareParams = booking.fareParams
      origAdditionalCategories = (.chargeCategory) <$> origFareParams.conditionalCharges
  let params =
        CalculateFareParametersParams
          { farePolicy = fullFarePolicy,
            actualDistance = booking.estimatedDistance,
            rideTime = booking.startTime,
            returnTime = booking.returnTime,
            roundTrip = fromMaybe False booking.roundTrip,
            waitingTime = Nothing,
            stopWaitingTimes = [],
            actualRideDuration = Nothing,
            vehicleAge = Nothing,
            driverSelectedFare = origFareParams.driverSelectedFare,
            customerExtraFee = origFareParams.customerExtraFee,
            petCharges = origFareParams.petCharges,
            nightShiftCharge = origFareParams.nightShiftCharge,
            customerCancellationDues = origFareParams.customerCancellationDues,
            estimatedCongestionCharge = booking.estimatedCongestionCharge,
            nightShiftOverlapChecking = nightShiftOverlapChecking,
            estimatedDistance = booking.estimatedDistance,
            estimatedRideDuration = booking.estimatedDuration,
            timeDiffFromUtc = Nothing,
            tollCharges = booking.tollCharges,
            noOfStops = length booking.stops,
            currency = booking.currency,
            distanceUnit = booking.distanceUnit,
            shouldApplyBusinessDiscount = False,
            shouldApplyPersonalDiscount = False,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            mbAdditonalChargeCategories = if null origAdditionalCategories then Nothing else Just origAdditionalCategories,
            numberOfLuggages = Just numberOfLuggages,
            govtChargesRate = Just transporterCfg.taxConfig.rideGst,
            pickupGateId = booking.pickupGateId
          }

  newFareParams <- FC.calculateFareParameters params
  let newEstimatedFare = fareSum newFareParams (Just [])

  QFP.create newFareParams
  QRB.updateNumberOfLuggagesAndFare booking.id (Just numberOfLuggages) newFareParams.id newEstimatedFare

  -- Send on_update to BAP so BAP mirrors the new luggage count + fare + breakup on its booking row
  sendAddBaggageUpdateToBAP booking numberOfLuggages newEstimatedFare newFareParams
handler (UEditLocationReq EditLocationReq {..}) = do
  when (isNothing origin' && isNothing destination') $
    throwError PickupOrDropLocationNotFound
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  when (ride.status == DRide.COMPLETED || ride.status == DRide.CANCELLED) $ throwError $ RideInvalidStatus ("Can't edit destination for completed/cancelled ride." <> Text.pack (show ride.status))
  let udf1 = if ride.status == DRide.INPROGRESS then "RIDE_INPROGRESS" else "RIDE_PICKUP"
  person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  let origin = mkLocation person.merchantOperatingCityId <$> origin'
  let destination = mkLocation person.merchantOperatingCityId <$> destination'
  whenJust origin $ \startLocation -> do
    QL.create startLocation
    pickupMapForBooking <- SLM.buildPickUpLocationMapping startLocation.id bookingId.getId DLM.BOOKING (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForBooking
    pickupMapForRide <- SLM.buildPickUpLocationMapping startLocation.id rideId.getId DLM.RIDE (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForRide
    searchReq <- SQSR.findByTransactionIdAndMerchantId transactionId person.merchantId >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
    pickupMapForSearchReq <- SLM.buildPickUpLocationMapping startLocation.id searchReq.id.getId DLM.SEARCH_REQUEST (Just person.merchantId) (Just person.merchantOperatingCityId)
    QLM.create pickupMapForSearchReq
    driverInfo <- QDI.findById person.id >>= fromMaybeM DriverInfoNotFound
    overlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory person.merchantOperatingCityId "EDIT_LOCATION" (fromMaybe ENGLISH person.language) Nothing Nothing (Just booking.configInExperimentVersions) >>= fromMaybeM (InternalError "Overlay not found for EDIT_LOCATION")
    let fcmOverlayReq = Notify.mkOverlayReq overlay
    let entityData = Notify.EditPickupLocationReq {hasAdvanceBooking = driverInfo.hasAdvanceBooking, ..}
    Notify.sendPickupLocationChangedOverlay person fcmOverlayReq entityData
    QRide.updateIsPickupOrDestinationEdited (Just True) ride.id

  whenJust destination $ \dropLocation -> do
    --------------------TO DO ----------------------- Dependency on other people changes
    -----------1. Add a check for forward dispatch ride -----------------
    -----------2. Add a check for last location timestamp of driver ----------------- LTS dependency
    hasAdvancedRide <- QDI.findById (cast ride.driverId) <&> maybe False (.hasAdvanceBooking)
    if hasAdvancedRide
      then sendUpdateEditDestErrToBAP booking bapBookingUpdateRequestId "Trip Update Request Not Available" "Driver has an upcoming ride near your drop location. "
      else do
        transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
        now <- getCurrentTime
        QL.create dropLocation
        let dropLatLong = Maps.LatLong {lat = dropLocation.lat, lon = dropLocation.lon}
        let srcPt = Maps.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
        let bookedsStops = booking.stops
        let stopLatLongs = map (\stop -> Maps.LatLong {lat = stop.lat, lon = stop.lon}) bookedsStops
        merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
        case status of
          Enums.SOFT_UPDATE -> do
            reachedStopsMap <- Redis.hGetAll (VID.mkReachedStopsKey ride.id) :: Flow [(Text, VID.ReachedStopInfo)]
            let reachedStopIndices = Set.fromList $ map ((.stopIndex) . snd) reachedStopsMap
                pendingStops = [latLong | (latLong, idx) <- zip stopLatLongs [1 ..], Set.notMember idx reachedStopIndices]
            (pickedWaypoints, currentPoint, snapToRoadFailed) <- computeSoftUpdateWaypoints rideId ride merchantOperatingCity srcPt dropLatLong pendingStops reachedStopsMap now
            bookingUpdateReq <-
              computeFareAndBuildSoftUpdateBUR
                booking
                merchantOperatingCity
                person
                transporterConfig
                ride
                pickedWaypoints
                currentPoint
                snapToRoadFailed
                srcPt
                dropLatLong
                bapBookingUpdateRequestId
                now
                (length booking.stops)
                Nothing
                DBUR.DESTINATION
            startLocMapping <- QLM.getLatestStartByEntityId bookingId.getId >>= fromMaybeM (InternalError $ "Latest start location mapping not found for bookingId: " <> bookingId.getId)
            dropLocMapping <- QLM.getLatestEndByEntityId bookingId.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingId: " <> bookingId.getId)
            startLocMap <- SLM.buildPickUpLocationMapping startLocMapping.locationId bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
            QLM.create startLocMap
            dropLocMap <- SLM.buildDropLocationMapping dropLocMapping.locationId bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
            QLM.create dropLocMap
            QBUR.create bookingUpdateReq
            QBURExtra.expireActiveSoftSiblings booking.id bookingUpdateReq.id
            destLocMapNew <- SLM.buildDropLocationMapping dropLocation.id bookingUpdateReq.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq.merchantId) (Just bookingUpdateReq.merchantOperatingCityId)
            QLM.create destLocMapNew
            sendUpdateEditDestToBAP booking ride bookingUpdateReq (Just dropLocation) currentPoint SOFT_UPDATE
          Enums.CONFIRM_UPDATE -> do
            bookingUpdateReq <- QBUR.findByBAPBUReqId bapBookingUpdateRequestId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with BAPBookingUpdateRequestId" <> bapBookingUpdateRequestId)
            when (bookingUpdateReq.validTill < now) $ throwError (InvalidRequest "BookingUpdateRequest is expired")
            when (bookingUpdateReq.status == DBUR.EXPIRED) $ throwError (InvalidRequest "BookingUpdateRequest was superseded by a newer edit")
            when (bookingUpdateReq.status /= DBUR.SOFT) $ throwError (InvalidRequest "BookingUpdateRequest is not in SOFT state")
            -- Supersede: mark all other active BURs for this booking as EXPIRED and notify BAP.
            -- Held under L1, so concurrent /update CONFIRMs serialize here.
            EditBooking.supersedeSiblingBURs booking bookingUpdateReq.id
            QBUR.updateStatusById DBUR.USER_CONFIRMED bookingUpdateReq.id
            EditBooking.snapshotSoftRouteToConfirm booking.id.getId (transporterConfig.editLocTimeThreshold.getSeconds + 60)
            if transporterConfig.editLocDriverPermissionNeeded
              then do
                newEstimatedDistance <- bookingUpdateReq.estimatedDistance & fromMaybeM (InternalError $ "No estimated distance found for bookingUpdateReq with Id :" <> bookingUpdateReq.id.getId)
                oldEstimatedDistance <- bookingUpdateReq.oldEstimatedDistance & fromMaybeM (InternalError $ "No estimated distance found for booking with Id :" <> booking.id.getId)
                let entityData =
                      Notify.UpdateLocationNotificationReq
                        { rideId = ride.id,
                          origin = Nothing,
                          destination = Just dropLocation,
                          stops = Nothing,
                          bookingUpdateRequestId = bookingUpdateReq.id,
                          newEstimatedDistance,
                          newEstimatedDistanceWithUnit = convertHighPrecMetersToDistance bookingUpdateReq.distanceUnit newEstimatedDistance,
                          newEstimatedFare = bookingUpdateReq.estimatedFare,
                          oldEstimatedDistance,
                          oldEstimatedDistanceWithUnit = convertHighPrecMetersToDistance bookingUpdateReq.distanceUnit oldEstimatedDistance,
                          oldEstimatedFare = bookingUpdateReq.oldEstimatedFare,
                          validTill = bookingUpdateReq.validTill
                        }
                mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory booking.merchantOperatingCityId "UPDATE_LOC_FCM" (fromMaybe ENGLISH person.language) (Just udf1) Nothing Nothing
                overlay <- case mbOverlay of
                  Just o -> pure o
                  Nothing -> CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory booking.merchantOperatingCityId "UPDATE_LOC_FCM" ENGLISH (Just udf1) Nothing Nothing >>= fromMaybeM (InternalError "Overlay not found for UPDATE_LOC_FCM")
                let locationLat = if ride.status == DRide.INPROGRESS then dropLocation.lat else ride.fromLocation.lat
                    locationLon = if ride.status == DRide.INPROGRESS then dropLocation.lon else ride.fromLocation.lon
                    actions2 = map (mkActions2 bookingUpdateReq.id.getId locationLat locationLon) overlay.actions2
                    secondaryActions2 = fmap (map (mkSecondaryActions2 bookingUpdateReq.id.getId)) overlay.secondaryActions2
                    overlay' = overlay{actions2, secondaryActions2}
                Notify.sendUpdateLocOverlay merchantOperatingCity.id person (Notify.mkOverlayReq overlay') entityData
              else void $ EditBooking.postEditResultInner (Just person.id, merchantOperatingCity.merchantId, merchantOperatingCity.id) bookingUpdateReq.id (EditBooking.EditBookingRespondAPIReq {action = EditBooking.ACCEPT}) (pure ())
          _ -> throwError (InvalidRequest "Invalid status for edit location request")

mkActions2 :: Text -> Double -> Double -> FCM.FCMActions -> FCM.FCMActions
mkActions2 bookingUpdateReqId lat long action = do
  let primaryAction' = case action.primaryAction of
        FCM.CALL_API details -> do
          let ep = T.replace (Notify.templateText "bookingUpdateRequestId") bookingUpdateReqId details.endPoint
          let details' = details{endPoint = ep}
          CALL_API details'
        FCM.NAVIGATE details -> do
          let details' = details{lat, long}
          NAVIGATE details'
        _ -> action.primaryAction
  let dependentActions' = map (mkActions2 bookingUpdateReqId lat long) action.dependentActions
  FCM.FCMActions {primaryAction = primaryAction', dependentActions = dependentActions'}

mkSecondaryActions2 :: Text -> FCM.FCMActions -> FCM.FCMActions
mkSecondaryActions2 bookingUpdateReqId action = do
  let primaryAction' = case action.primaryAction of
        FCM.CALL_API details -> do
          let ep = T.replace (Notify.templateText "bookingUpdateRequestId") bookingUpdateReqId details.endPoint
          let details' = details{endPoint = ep}
          CALL_API details'
        _ -> action.primaryAction
  let dependentActions' = map (mkSecondaryActions2 bookingUpdateReqId) action.dependentActions
  FCM.FCMActions {primaryAction = primaryAction', dependentActions = dependentActions'}

handleStopsModification :: DBooking.Booking -> DRide.Ride -> Id DRide.Ride -> Enums.OrderStatus -> [DL.Location'] -> Text -> Maybe Int -> DP.Person -> Flow ()
handleStopsModification booking ride rideId status newStopLocs' bapBookingUpdateRequestId mbPreservedPrefixStops person = do
  let newStops = map (mkLocation booking.merchantOperatingCityId) newStopLocs'

  merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  case status of
    Enums.SOFT_UPDATE -> do
      n <- mbPreservedPrefixStops & fromMaybeM (InvalidRequest "PRESERVED_PREFIX_STOPS tag is required for EDIT_STOPS SOFT_UPDATE")
      reachedStopsMap <- Redis.hGetAll (VID.mkReachedStopsKey ride.id) :: Flow [(Text, VID.ReachedStopInfo)]
      let reachedStopIndices = Set.fromList $ map ((.stopIndex) . snd) reachedStopsMap
      when (any (> n) reachedStopIndices) $ do
        sendUpdateEditDestErrToBAP booking bapBookingUpdateRequestId "Cannot modify already-reached stops" ""
        throwError $ InvalidRequest "Cannot modify already-reached stops"
      let srcPt = Maps.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
          bookedStopLatLongs = map (\s -> Maps.LatLong {lat = s.lat, lon = s.lon}) booking.stops
          unchangedPendingStops = [ll | (ll, idx) <- zip bookedStopLatLongs [1 ..], Set.notMember idx reachedStopIndices, idx <= n]
          newTailLatLongs = map (\s -> Maps.LatLong {lat = s.lat, lon = s.lon}) newStops
          dropLatLong = maybe srcPt (\loc -> Maps.LatLong {lat = loc.lat, lon = loc.lon}) booking.toLocation
          pendingStops = unchangedPendingStops ++ newTailLatLongs
      now <- getCurrentTime
      (pickedWaypoints, currentPoint, snapToRoadFailed) <- computeSoftUpdateWaypoints rideId ride merchantOperatingCity srcPt dropLatLong pendingStops reachedStopsMap now

      transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      bookingUpdateReq' <-
        computeFareAndBuildSoftUpdateBUR
          booking
          merchantOperatingCity
          person
          transporterConfig
          ride
          pickedWaypoints
          currentPoint
          snapToRoadFailed
          srcPt
          dropLatLong
          bapBookingUpdateRequestId
          now
          (n + length newStops)
          (Just n)
          DBUR.STOPS
      QL.createMany newStops
      stopMappings <- mapM (\(loc, stopOrd) -> SLM.buildStopLocationMapping loc bookingUpdateReq'.id.getId DLM.BOOKING_UPDATE_REQUEST (Just bookingUpdateReq'.merchantId) (Just bookingUpdateReq'.merchantOperatingCityId) stopOrd) (zip newStops [(n + 1) ..])
      QLM.createMany stopMappings
      QBUR.create bookingUpdateReq'
      QBURExtra.expireActiveSoftSiblings booking.id bookingUpdateReq'.id
      sendUpdateEditDestToBAP booking ride bookingUpdateReq' booking.toLocation currentPoint SOFT_UPDATE
    Enums.CONFIRM_UPDATE -> do
      now <- getCurrentTime
      bookingUpdateReq <- QBUR.findByBAPBUReqId bapBookingUpdateRequestId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with BAPBookingUpdateRequestId: " <> bapBookingUpdateRequestId)
      when (bookingUpdateReq.validTill < now) $ throwError (InvalidRequest "BookingUpdateRequest is expired")
      when (bookingUpdateReq.status == DBUR.EXPIRED) $ throwError (InvalidRequest "BookingUpdateRequest was superseded by a newer edit")
      when (bookingUpdateReq.status /= DBUR.SOFT) $ throwError (InvalidRequest "BookingUpdateRequest is not in SOFT state")
      n <- bookingUpdateReq.preservedPrefixStops & fromMaybeM (InternalError $ "preservedPrefixStops missing on EDIT_STOPS BUR " <> bookingUpdateReq.id.getId)
      -- Supersede: mark all other active BURs for this booking as EXPIRED and notify BAP.
      EditBooking.supersedeSiblingBURs booking bookingUpdateReq.id
      when (ride.status == DRide.INPROGRESS) $
        VID.ensureNoReachedStopsBeyond ride.id
          n
          "Stale state: a stop in the unchanged zone has been reached since the update was requested. Please retry."
      QBUR.updateStatusById DBUR.USER_CONFIRMED bookingUpdateReq.id
      transporterConfig <-
        SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (DriverId (cast person.id)))
          >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      EditBooking.snapshotSoftRouteToConfirm booking.id.getId (transporterConfig.editLocTimeThreshold.getSeconds + 60)
      if transporterConfig.editLocDriverPermissionNeeded
        then do
          newEstimatedDistance <-
            bookingUpdateReq.estimatedDistance
              & fromMaybeM (InternalError $ "No estimated distance found for bookingUpdateReq with Id :" <> bookingUpdateReq.id.getId)
          oldEstimatedDistance <-
            bookingUpdateReq.oldEstimatedDistance
              & fromMaybeM (InternalError $ "No estimated distance found for booking with Id :" <> booking.id.getId)
          let stopLocs = take n booking.stops ++ newStops
          let entityData =
                Notify.UpdateLocationNotificationReq
                  { rideId = ride.id,
                    origin = Nothing,
                    destination = Nothing,
                    stops = Just stopLocs,
                    bookingUpdateRequestId = bookingUpdateReq.id,
                    newEstimatedDistance,
                    newEstimatedDistanceWithUnit = convertHighPrecMetersToDistance bookingUpdateReq.distanceUnit newEstimatedDistance,
                    newEstimatedFare = bookingUpdateReq.estimatedFare,
                    oldEstimatedDistance,
                    oldEstimatedDistanceWithUnit = convertHighPrecMetersToDistance bookingUpdateReq.distanceUnit oldEstimatedDistance,
                    oldEstimatedFare = bookingUpdateReq.oldEstimatedFare,
                    validTill = bookingUpdateReq.validTill
                  }
          let udf1Stops = if ride.status == DRide.INPROGRESS then "RIDE_INPROGRESS" else "RIDE_PICKUP"
          mbOverlay <-
            CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
              booking.merchantOperatingCityId
              "UPDATE_LOC_FCM"
              (fromMaybe ENGLISH person.language)
              (Just udf1Stops)
              Nothing
              Nothing
          overlay <- case mbOverlay of
            Just o -> pure o
            Nothing ->
              CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
                booking.merchantOperatingCityId
                "UPDATE_LOC_FCM"
                ENGLISH
                (Just udf1Stops)
                Nothing
                Nothing
                >>= fromMaybeM (InternalError "Overlay not found for UPDATE_LOC_FCM")
          let (locationLat, locationLon) = case stopLocs of
                (s : _) -> (s.lat, s.lon)
                [] -> (ride.fromLocation.lat, ride.fromLocation.lon)
              actions2 = map (mkActions2 bookingUpdateReq.id.getId locationLat locationLon) overlay.actions2
              secondaryActions2 = fmap (map (mkSecondaryActions2 bookingUpdateReq.id.getId)) overlay.secondaryActions2
              overlay' = overlay{actions2, secondaryActions2}
          Notify.sendUpdateLocOverlay merchantOperatingCity.id person (Notify.mkOverlayReq overlay') entityData
        else
          void $
            EditBooking.postEditResultInner
              (Just person.id, merchantOperatingCity.merchantId, merchantOperatingCity.id)
              bookingUpdateReq.id
              (EditBooking.EditBookingRespondAPIReq {action = EditBooking.ACCEPT})
              (pure ())
    _ -> throwError (InvalidRequest "Invalid status for edit stops request")

computeSoftUpdateWaypoints ::
  Id DRide.Ride ->
  DRide.Ride ->
  DMOC.MerchantOperatingCity ->
  Maps.LatLong ->
  Maps.LatLong ->
  [Maps.LatLong] ->
  [(Text, VID.ReachedStopInfo)] ->
  UTCTime ->
  Flow (NonEmpty Maps.LatLong, Maybe Maps.LatLong, Maybe Bool)
computeSoftUpdateWaypoints rideId ride merchantOperatingCity srcPt dropLatLong pendingStops reachedStopsMap now = do
  if ride.status == DRide.INPROGRESS
    then do
      currentLocationPointsBatch <- LTS.driverLocation rideId merchantOperatingCity.merchantId ride.driverId
      logTagError "DebugErrorLog: EditDestSoftUpdate" $ "driver location points count: " <> show (length currentLocationPointsBatch.loc) <> ", driverId=" <> ride.driverId.getId <> ", rideId=" <> rideId.getId
      editDestinationWaypoints <- getEditDestinationWaypoints ride.driverId
      logTagError "DebugErrorLog: EditDestSoftUpdate" $ "edit destination waypoints count: " <> show (length editDestinationWaypoints)
      alreadySnappedPoints <- getEditDestinationSnappedWaypoints ride.driverId
      let startPoint = if length alreadySnappedPoints > 0 then (fst $ last alreadySnappedPoints) else (Maps.LatLong ride.fromLocation.lat ride.fromLocation.lon)
      let (currentLocPoint :: Maps.LatLong) =
            fromMaybe startPoint $
              (if not $ null currentLocationPointsBatch.loc then let w = last currentLocationPointsBatch.loc in Just (Maps.LatLong w.lat w.lon) else Nothing)
                <|> (if not $ null editDestinationWaypoints then Just (fst $ last editDestinationWaypoints) else Nothing)

      logTagError "DebugErrorLog: EditDestSoftUpdate" $ "Already snapped points count: " <> show (length alreadySnappedPoints)
      processedStopIndices <- Set.fromList <$> Redis.sMembers (VID.mkProcessedStopsKey ride.id)
      let nowTs = floor $ utcTimeToPOSIXSeconds now
          reachedStops = sortOn (.timestamp) $ map snd reachedStopsMap
          unprocessedReachedStops = filter (\r -> Set.notMember r.stopIndex processedStopIndices) reachedStops
          rawGpsTriples = map (\(ll, t) -> (ll, False, t)) (editDestinationWaypoints <> map (\w -> (Maps.LatLong w.lat w.lon, fromMaybe nowTs w.ts)) currentLocationPointsBatch.loc)
      unless (null unprocessedReachedStops) $ do
        void $ Redis.sAddExp (VID.mkProcessedStopsKey ride.id) (map VID.stopIndex unprocessedReachedStops) 86400
      let mergedRawPath = mergeReachedStopsChronologically rawGpsTriples unprocessedReachedStops
          segments = splitAtStopMarkers mergedRawPath
      logTagError "DebugErrorLog: EditDestSoftUpdate" $ "mergedRawPath length (before snap): " <> show (length mergedRawPath)
      logTagError "DebugErrorLog: EditDestSoftUpdate" $ "Number of segments to snap: " <> show (length segments)
      -- Invariant: each segment contains at most 1 stop point,
      -- and if present, the stop is always the last element of the segment.
      results <- forM segments $ \seg -> do
        let stopLLs = [(ll, True) | (ll, True, _) <- seg]
            nonStopLLs = [ll | (ll, False, _) <- seg]
        (segFailed, snapped) <- getLatlongsViaSnapToRoad nonStopLLs
        return (segFailed, map (,False) snapped ++ stopLLs)
      let (failures, snappedSegList) = unzip results
          snapToRoadFailed = or failures
          editDestinationPoints = concat snappedSegList
      logTagError "DebugErrorLog: EditDestSoftUpdate" $ "snapped edit destination points count (after snap): " <> show (length editDestinationPoints)
      let currentPoint = if snapToRoadFailed || null editDestinationPoints then currentLocPoint else fst (last editDestinationPoints)
          finalMergedPath = alreadySnappedPoints <> (editDestinationPoints <> [(currentPoint, True)])
      whenJust (nonEmpty finalMergedPath) $ \finalMergedPath' -> do
        deleteAndPushEditDestinationSnappedWayPoints ride.driverId finalMergedPath'
      deleteEditDestinationWaypoints ride.driverId
      let snappedCount = max 0 (8 - length pendingStops)
      return (srcPt :| (pickedWaypointsForEditDestination finalMergedPath snappedCount ++ pendingStops ++ [dropLatLong]), Just currentPoint, Just snapToRoadFailed)
    else return (srcPt :| (pendingStops ++ [dropLatLong]), Nothing, Nothing)
  where
    snapToRoad latlongs = do
      res <- Maps.snapToRoadWithFallback Nothing merchantOperatingCity.merchantId merchantOperatingCity.id True (Just rideId.getId) Maps.SnapToRoadReq {points = latlongs, distanceUnit = Meter, calculateDistanceFrom = Nothing}
      case res of
        (_, Left e) -> do
          logTagError "snapToRoadWithFallback failed in edit destination" $ "Error: " <> show e
          return (True, [])
        (_, Right snapToRoadResp) ->
          pure (False, snapToRoadResp.snappedPoints)

    getLatlongsViaSnapToRoad latlongs = do
      let batches = chunksOf 98 latlongs
      logTagError "DebugErrorLog: EditDestSnapToRoad" $ "SnapToRoad request batch count: " <> show (length batches)
      results <- forM batches $ \batch -> do
        logTagError "DebugErrorLog: EditDestSnapToRoad" $ "Processing snap chunk with point count: " <> show (length batch)
        res@(_, snappedBatch) <- snapToRoad batch
        logTagError "DebugErrorLog: EditDestSnapToRoad" $ "SnapToRoad snapped chunk point count: " <> show (length snappedBatch)
        return res
      let (failures, snappedBatches) = unzip results
      return (or failures, concat snappedBatches)
    mergeReachedStopsChronologically snappedPath reachedStops =
      go snappedPath reachedStops
      where
        go [] [] = []
        go path [] = path
        go [] stops = map (\stop -> (stop.location, True, stop.timestamp)) stops
        go path@((ll, b, t) : restPath) stops@(stop : restStops)
          | stop.timestamp <= t = (stop.location, True, stop.timestamp) : go path restStops
          | otherwise = (ll, b, t) : go restPath stops

    splitAtStopMarkers [] = []
    splitAtStopMarkers pts = go pts []
      where
        go [] acc = [reverse acc]
        go ((ll, True, t) : rest) acc = (reverse ((ll, True, t) : acc)) : go rest []
        go (pt : rest) acc = go rest (pt : acc)

buildLocation :: MonadFlow m => Id DM.Merchant -> Maybe (Id DMOC.MerchantOperatingCity) -> Common.Location -> m DL.Location
buildLocation merchantId mbMerchantOperatingCityId location = do
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
              fullAddress = mkFullAddress location.address,
              instructions = Nothing,
              extras = Nothing
            },
        merchantId = Just merchantId,
        merchantOperatingCityId = mbMerchantOperatingCityId
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

processStop :: DBooking.Booking -> DL.Location -> Bool -> Flow ()
processStop booking location isEdit = do
  validateStopReq booking isEdit
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.providerId) (Just booking.merchantOperatingCityId)
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking.id (Just location.id.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  whenJust mbRide $ \ride -> do
    person <- runInReplica $ QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    let entityData = Notify.StopReq {bookingId = booking.id, stop = Just (DL.makeLocationAPIEntity location), ..}
    when (ride.status `elem` [DRide.INPROGRESS, DRide.NEW]) $ Notify.notifyStopModification ride.status person entityData booking.tripCategory -- when ride.status in [INPROGRESS, NEW]

validateStopReq :: DBooking.Booking -> Bool -> Flow ()
validateStopReq booking isEdit = do
  unless (booking.status `elem` [DBooking.NEW, DBooking.TRIP_ASSIGNED]) $ throwError $ BookingInvalidStatus ("Cannot add stop in this booking " <> booking.id.getId) -- check for rental?
  if isEdit
    then unless (isJust booking.stopLocationId) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> booking.id.getId) -- should we throw error or just allow?
    else unless (isNothing booking.stopLocationId) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> booking.id.getId)

buildbookingUpdateRequest :: MonadFlow m => DBooking.Booking -> Id DM.Merchant -> Text -> DFP.FareParameters -> Id DFP.FarePolicy -> Maybe Meters -> Maybe Maps.LatLong -> Meters -> UTCTime -> Text -> Text -> Maybe Bool -> Maybe Int -> Maybe DBUR.BookingUpdateType -> m DBUR.BookingUpdateRequest
buildbookingUpdateRequest booking merchantId bapBookingUpdateRequestId fareParams farePolicyId maxEstimatedDistance currentPoint estimatedDistance validTill mapsRouteReqInText routeInfoInText snapToRoadFailed preservedPrefixStops updateType = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DBUR.BookingUpdateRequest
      { id = guid,
        status = DBUR.SOFT,
        bapBookingUpdateRequestId,
        createdAt = now,
        updatedAt = now,
        bookingId = booking.id,
        merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        currentPointLat = (.lat) <$> currentPoint,
        currentPointLon = (.lon) <$> currentPoint,
        estimatedFare = HighPrecMoney $ toRational $ fareSum fareParams Nothing,
        estimatedDistance = Just $ metersToHighPrecMeters estimatedDistance,
        oldEstimatedFare = booking.estimatedFare,
        maxEstimatedDistance = metersToHighPrecMeters <$> maxEstimatedDistance,
        oldEstimatedDistance = metersToHighPrecMeters <$> booking.estimatedDistance,
        totalDistance = Nothing,
        travelledDistance = Nothing,
        fareParamsId = fareParams.id,
        oldFareParamsId = booking.fareParams.id,
        oldMaxEstimatedDistance = booking.maxEstimatedDistance,
        validTill,
        distanceUnit = booking.distanceUnit,
        getRouteReq = Just mapsRouteReqInText,
        routeInfoResp = Just routeInfoInText,
        preservedPrefixStops,
        updateType,
        ..
      }

computeFareAndBuildSoftUpdateBUR ::
  DBooking.Booking ->
  DMOC.MerchantOperatingCity ->
  DP.Person ->
  TransporterConfig ->
  DRide.Ride ->
  NonEmpty Maps.LatLong ->
  Maybe Maps.LatLong ->
  Maybe Bool ->
  Maps.LatLong ->
  Maps.LatLong ->
  Text ->
  UTCTime ->
  Int ->
  Maybe Int ->
  DBUR.BookingUpdateType ->
  Flow DBUR.BookingUpdateRequest
computeFareAndBuildSoftUpdateBUR booking mOCity person transporterConfig ride pickedWaypoints currentPoint snapToRoadFailed srcPt dropLatLong bapBookingUpdateRequestId now noOfStops mbPreservedPrefixStops updateType = do
  logTagInfo "update Ride soft update" $ "pickedWaypoints: " <> show pickedWaypoints
  routeResponse <-
    Maps.getRoutes mOCity.merchantId mOCity.id (Just ride.id.getId) $
      Maps.GetRoutesReq {waypoints = pickedWaypoints, mode = Just Maps.CAR, calcPoints = True}
  shortestRoute <- getRouteInfoWithShortestDuration routeResponse & fromMaybeM (InternalError "No route found for soft update")
  let maxEstimatedDist = maybe Nothing (.distance) (Maps.getLongestRouteDistance routeResponse)
  estimatedDistance <- shortestRoute.distance & fromMaybeM (InternalError "No distance found for soft update route")
  (duration :: Seconds) <- shortestRoute.duration & fromMaybeM (InternalError "No duration found for soft update route")
  let routeInfo = RR.RouteInfo {distance = Just estimatedDistance, distanceWithUnit = Just (convertMetersToDistance booking.distanceUnit estimatedDistance), duration = Just duration, points = Just shortestRoute.points}
      mapsRouteReqInText = T.pack $ show Maps.GetRoutesReq {waypoints = pickedWaypoints, mode = Just Maps.CAR, calcPoints = True}
      routeInfoInText = T.pack $ show routeInfo
  let cacheTtl = transporterConfig.editLocTimeThreshold.getSeconds + 60
  Redis.setExp (bookingRequestKeySoftUpdate booking.id.getId) routeInfo cacheTtl
  Redis.setExp (multipleRouteKeySoftUpdate booking.id.getId) (map RR.createMultipleRouteInfo routeResponse) cacheTtl
  mbTollInfo <- getTollInfoOnRoute mOCity.id.getId (Just person.id.getId) shortestRoute.points
  let isTollAllowed =
        maybe
          True
          ( \(_, _, _, isAutoRickshawAllowed, isTwoWheelerAllowed) ->
              ((booking.vehicleServiceTier == DVST.AUTO_RICKSHAW || booking.vehicleServiceTier == DVST.AUTO_PLUS) && isAutoRickshawAllowed)
                || (booking.vehicleServiceTier == DVST.BIKE && fromMaybe False isTwoWheelerAllowed)
                || (booking.vehicleServiceTier /= DVST.AUTO_RICKSHAW && booking.vehicleServiceTier /= DVST.AUTO_PLUS && booking.vehicleServiceTier /= DVST.BIKE)
          )
          mbTollInfo
  when (not isTollAllowed) $ do
    sendUpdateEditDestErrToBAP booking bapBookingUpdateRequestId "Trip Update Request Not Available" "Vehicle not allowed for toll route."
    throwError $ InvalidRequest "Vehicle not allowed for toll route."
  fareProducts <- getAllFarePoliciesProduct mOCity.merchantId mOCity.id False srcPt (Just dropLatLong) Nothing Nothing (Just (TransactionId (Id booking.transactionId))) booking.fromLocGeohash booking.toLocGeohash (Just estimatedDistance) (Just duration) booking.dynamicPricingLogicVersion booking.tripCategory booking.configInExperimentVersions
  farePolicy <- getFarePolicy (Just srcPt) (Just dropLatLong) booking.fromLocGeohash booking.toLocGeohash (Just estimatedDistance) (Just duration) mOCity.id False booking.tripCategory booking.vehicleServiceTier (Just fareProducts.area) (Just booking.startTime) booking.dynamicPricingLogicVersion (Just (TransactionId (Id booking.transactionId))) booking.configInExperimentVersions booking.specialLocationName
  mbDomainDiscountPct <- CQDDC.resolveDomainDiscountPercentage booking.merchantOperatingCityId booking.emailDomain booking.businessEmailDomain booking.billingCategory farePolicy.vehicleServiceTier
  let farePolicy' =
        farePolicy
          { DFP.businessDiscountPercentage = mbDomainDiscountPct <|> farePolicy.businessDiscountPercentage,
            DFP.personalDiscountPercentage = mbDomainDiscountPct <|> farePolicy.personalDiscountPercentage
          } ::
          DFP.FullFarePolicy
  fareParameters <-
    FC.calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = farePolicy',
          actualDistance = Just estimatedDistance,
          rideTime = booking.startTime,
          returnTime = booking.returnTime,
          vehicleAge = Nothing,
          roundTrip = fromMaybe False booking.roundTrip,
          waitingTime = Nothing,
          stopWaitingTimes = [],
          noOfStops = noOfStops,
          actualRideDuration = Nothing,
          driverSelectedFare = booking.fareParams.driverSelectedFare,
          petCharges = booking.fareParams.petCharges,
          customerExtraFee = booking.fareParams.customerExtraFee,
          nightShiftCharge = booking.fareParams.nightShiftCharge,
          customerCancellationDues = booking.fareParams.customerCancellationDues,
          nightShiftOverlapChecking = False,
          estimatedDistance = Just estimatedDistance,
          estimatedRideDuration = Just duration,
          timeDiffFromUtc = Nothing,
          shouldApplyBusinessDiscount = booking.billingCategory == SLT.BUSINESS,
          shouldApplyPersonalDiscount = booking.billingCategory == SLT.PERSONAL,
          tollCharges = mbTollInfo <&> (\(tollCharges, _, _, _, _) -> tollCharges),
          currency = booking.currency,
          distanceUnit = booking.distanceUnit,
          estimatedCongestionCharge = booking.estimatedCongestionCharge,
          merchantOperatingCityId = Just booking.merchantOperatingCityId,
          mbAdditonalChargeCategories = Just $ map (.chargeCategory) booking.fareParams.conditionalCharges,
          numberOfLuggages = booking.numberOfLuggages,
          govtChargesRate = Just transporterConfig.taxConfig.rideGst,
          pickupGateId = booking.pickupGateId
        }
  QFP.create fareParameters
  let validTill = addUTCTime (fromIntegral transporterConfig.editLocTimeThreshold) now
  buildbookingUpdateRequest booking mOCity.merchantId bapBookingUpdateRequestId fareParameters farePolicy.id maxEstimatedDist currentPoint estimatedDistance validTill mapsRouteReqInText routeInfoInText snapToRoadFailed mbPreservedPrefixStops (Just updateType)
