{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One-shot assignment (dev/docs/one-shot-assign-plan.md): for enabled value-add-NP
-- BAPs with auto-assign, the driver's accept creates booking + ride here in one
-- synchronous sequence and informs the BAP through a single internal API call,
-- replacing the on_select -> init -> on_init -> confirm -> on_confirm relay.
-- Rows are written once in their final state (booking is born TRIP_ASSIGNED); the
-- only kept transition is SearchTry ACTIVE -> COMPLETED, the cross-driver gate.
module SharedLogic.OneShotAssign
  ( OneShotAssignReq (..),
    oneShotAssign,
  )
where

import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OnUpdate as DOU
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Booking (cancelBookingSilentToBAP)
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.Cancel (mkCancelSearchInitLockKey)
import SharedLogic.FareCalculator (mkFareParamsBreakups)
import SharedLogic.Ride (deactivateExistingQuotes, initializeRide)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event

data OneShotAssignReq = OneShotAssignReq
  { merchant :: DM.Merchant,
    searchReq :: DSR.SearchRequest,
    searchTry :: DST.SearchTry,
    driverQuote :: DDQ.DriverQuote,
    driver :: DPerson.Person,
    clientId :: Maybe Text,
    transporterConfig :: TransporterConfig
  }

oneShotAssign :: OneShotAssignReq -> Flow ()
oneShotAssign OneShotAssignReq {..} = do
  now <- getCurrentTime
  -- Same lock Beckn init takes: a concurrent cancel-search deactivates the quote after
  -- this lock releases, so holding it means the assignment below cannot race the cancel.
  isLockAcquired <- Redis.tryLockRedis (mkCancelSearchInitLockKey searchReq.transactionId) 30
  unless isLockAcquired $ throwError CustomerCancelled
  result <- withTryCatch "oneShotAssign" $ do
    let bArgs =
          DInit.BuildBookingReq
            { merchantId = merchant.id,
              bapId = searchReq.bapId,
              bapCity = searchReq.bapCity,
              bapCountry = searchReq.bapCountry,
              maxEstimatedDistance = Nothing,
              estimateId = driverQuote.estimateId.getId,
              isInsured = Nothing,
              insuredAmount = Nothing,
              displayBookingId = Nothing,
              discountAmount = Nothing,
              initialStatus = DRB.TRIP_ASSIGNED,
              riderId = searchReq.riderId,
              riderName = searchReq.riderName
            }
    booking <- DInit.buildBooking bArgs searchReq driverQuote searchTry.billingCategory driverQuote.id.getId driverQuote.tripCategory now Nothing Nothing (Just driverQuote.distanceToPickup) Nothing searchReq.configInExperimentVersions driverQuote.coinsRewardedOnGoldTierRide driverQuote.preferenceMatchScore (Just driverQuote.searchTryId) (Just driverQuote.durationToPickup) searchTry.emailDomain searchTry.businessEmailDomain driverQuote.isAutoAccepted
    triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = merchant.id}
    QRB.createBooking booking
    -- Assignment gate, same transition Beckn init performs: any driver responding after
    -- this gets RideRequestAlreadyAccepted from respondQuote.
    QST.updateStatus DST.COMPLETED searchTry.id
    -- From here the booking row exists: any failure must cancel it (silently — the BAP
    -- never learned of it) and expire the customer's search, mirroring the Beckn
    -- confirm errHandler, or we'd strand an orphaned TRIP_ASSIGNED booking.
    postBookingResult <- withTryCatch "oneShotAssign:postBooking" $ do
      mFleetAssociation <- QFDA.findByDriverId driver.id True
      (ride, _rideDetails, vehicle) <- initializeRide merchant driver booking Nothing Nothing clientId Nothing (mFleetAssociation <&> (.fleetOwnerId) <&> Id) True True
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId (mkPrice (Just driverQuote.currency) driverQuote.estimatedFare) (Just transporterConfig)
      fork "one-shot assign callback to BAP" $ do
        callbackResult <- withTryCatch "oneShotAssignCallback" $ do
          payload <- buildOneShotAssignPayload booking ride driver vehicle driverQuote
          appBackendBapInternal <- asks (.appBackendBapInternal)
          void $ withShortRetry $ CallBAPInternal.oneShotAssign appBackendBapInternal.apiKey appBackendBapInternal.url payload
        case callbackResult of
          Right _ -> logInfo $ "One-shot assign callback delivered for booking " <> booking.id.getId
          Left err -> do
            logError $ "One-shot assign callback failed for booking " <> booking.id.getId <> ", cancelling: " <> show err
            abortOneShotBooking booking
    case postBookingResult of
      Right _ -> pure ()
      Left err -> do
        logError $ "One-shot assign failed after booking creation for booking " <> booking.id.getId <> ", cancelling: " <> show err
        abortOneShotBooking booking
        throwM err
  Redis.unlockRedis (mkCancelSearchInitLockKey searchReq.transactionId)
  either throwM pure result
  where
    abortOneShotBooking booking = do
      -- Guarded so a cancel failure (lock contention etc.) can't skip the search-expired
      -- signal below and leave the customer waiting forever.
      cancelResult <- withTryCatch "oneShotAssignCancelBooking" $ cancelBookingSilentToBAP booking (Just driver) merchant
      case cancelResult of
        Left cancelErr -> logError $ "One-shot assign: cancelling booking " <> booking.id.getId <> " failed: " <> show cancelErr
        Right _ -> pure ()
      appBackendBapInternal <- asks (.appBackendBapInternal)
      void $
        withTryCatch "oneShotAssignRideSearchExpired" $
          CallBAPInternal.rideSearchExpired appBackendBapInternal.apiKey appBackendBapInternal.url (CallBAPInternal.RideSearchExpiredReq {transactionId = searchReq.transactionId})

-- | Derives the internal payload from the same builder the Beckn on_confirm/on_update
-- RIDE_ASSIGNED paths use (rideAssignedCommon), so driver image, birthday, favourites,
-- tier upgrade and vehicle-model refill behave identically in both flows.
buildOneShotAssignPayload :: DRB.Booking -> DRide.Ride -> DPerson.Person -> DVeh.Vehicle -> DDQ.DriverQuote -> Flow CallBAPInternal.OneShotAssignReq
buildOneShotAssignPayload booking ride driver vehicle driverQuote = do
  buildReq <- BP.rideAssignedCommon booking ride driver vehicle
  rideAssignedReq <- case buildReq of
    DOU.RideAssignedBuildReq r -> pure r
    DOU.ScheduledRideAssignedBuildReq r -> pure r
    _ -> throwError $ InternalError "rideAssignedCommon returned an unexpected build request"
  let bookingDetails = rideAssignedReq.bookingDetails
  driverName <- SP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
  driverMobile <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  let fareBreakups = mkFareParamsBreakups True identity CallBAPInternal.OneShotFareBreakupItem booking.fareParams
  pure
    CallBAPInternal.OneShotAssignReq
      { transactionId = booking.transactionId,
        bppEstimateId = driverQuote.estimateId.getId,
        bppQuoteId = driverQuote.id.getId,
        bppBookingId = booking.id.getId,
        bppRideId = ride.id.getId,
        currency = booking.currency,
        estimatedFare = booking.estimatedFare,
        commission = booking.commission,
        paymentCharge = booking.paymentCharge,
        paymentChargeBearer = booking.paymentChargeBearer,
        fareBreakups = fareBreakups,
        quoteValidTill = driverQuote.validTill,
        otp = fromMaybe ride.otp ride.endOtp,
        driverDetails =
          CallBAPInternal.OneShotDriverDetails
            { name = driverName,
              mobileCountryCode = driver.mobileCountryCode,
              mobileNumber = driverMobile,
              rating = SP.roundToOneDecimal <$> bookingDetails.driverStats.rating,
              registeredAt = Just driver.createdAt,
              image = rideAssignedReq.image,
              isDriverBirthDay = rideAssignedReq.isDriverBirthDay,
              accountId = Nothing -- online payment is out of one-shot scope (phase 1 is cash/postpaid)
            },
        vehicleDetails =
          CallBAPInternal.OneShotVehicleDetails
            { number = bookingDetails.vehicle.registrationNo,
              color = Just bookingDetails.vehicle.color,
              model = Just bookingDetails.vehicle.model,
              variant = bookingDetails.vehicle.variant,
              serviceTierType = booking.vehicleServiceTier,
              serviceTierName = Just booking.vehicleServiceTierName,
              vehicleAge = rideAssignedReq.vehicleAge
            },
        distanceToPickup = Just driverQuote.distanceToPickup,
        durationToPickup = Just driverQuote.durationToPickup,
        previousRideEndPos = ride.previousRideTripEndPos,
        isAlreadyFav = rideAssignedReq.isAlreadyFav,
        favCount = Just rideAssignedReq.favCount,
        isSafetyPlus = rideAssignedReq.isSafetyPlus,
        isFreeRide = rideAssignedReq.isFreeRide,
        specialLocationTag = booking.specialLocationTag,
        isTierUpgrade = rideAssignedReq.isTierUpgrade,
        assignedServiceTierName = rideAssignedReq.assignedServiceTierName,
        billingCategory = booking.billingCategory
      }
