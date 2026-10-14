{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Completes a "find a better driver" swap when a replacement driver accepts.
--
-- The Beckn relay cannot do this: on_select reaches a BAP still holding the rider's live
-- TRIP_ASSIGNED booking, whose confirm rejects it (ACTIVE_BOOKING_ALREADY_PRESENT). So
-- this replaces sendDriverOffer - the same hook SharedLogic.OneShotAssign uses - creating
-- booking + ride here and telling the BAP via the one-shot callback (oldBppBookingId set).
module SharedLogic.BetterDriverSwap
  ( BetterDriverSwapReq (..),
    betterDriverSwapAssign,
  )
where

import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Action.UI.Ride.Common as RideCommon
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as DTCR
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Domain.Types.TransporterConfig (TransporterConfig)
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Booking (cancelBooking)
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.Cancel (mkCancelSearchInitLockKey)
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.OneShotAssign (buildAssignedDriverRideRes, buildOneShotAssignPayload)
import SharedLogic.QuickRetry (withQuickRetry)
import SharedLogic.Ride (deactivateExistingQuotes, initializeRide)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics

data BetterDriverSwapReq = BetterDriverSwapReq
  { merchant :: DM.Merchant,
    searchReq :: DSR.SearchRequest,
    searchTry :: DST.SearchTry,
    driverQuote :: DDQ.DriverQuote,
    driver :: DPerson.Person,
    driverStats :: DStats.DriverStats,
    driverInfo :: DDI.DriverInformation,
    clientId :: Maybe Text,
    transporterConfig :: TransporterConfig
  }

betterDriverSwapAssign :: BetterDriverSwapReq -> Flow (Maybe RideCommon.DriverRideRes)
betterDriverSwapAssign BetterDriverSwapReq {..} = do
  now <- getCurrentTime
  oldBookingIdText <-
    searchTry.standInForBookingId
      & fromMaybeM (InternalError $ "BETTER_DRIVER_SEARCH searchTry " <> searchTry.id.getId <> " has no standInForBookingId")
  oldBooking <- QRB.findById (Id oldBookingIdText) >>= fromMaybeM (BookingDoesNotExist oldBookingIdText)
  oldRide <- QRide.findActiveByRBId oldBooking.id >>= fromMaybeM (RideNotFound $ "no active ride for old booking: " <> oldBookingIdText)
  -- Same lock Beckn init takes: a concurrent cancel-search would deactivate the quote.
  isLockAcquired <- Redis.tryLockRedis (mkCancelSearchInitLockKey searchReq.transactionId) 30
  unless isLockAcquired $ throwError CustomerCancelled
  result <- withTryCatch "betterDriverSwapAssign" $ do
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
    mbRiderConsent <- maybe (pure Nothing) (fmap (>>= (.consentToShareMobileNumber)) . QRD.findById) searchReq.riderId
    booking <- (\b -> b {DRB.numberShareConsent = mbRiderConsent}) <$> DInit.buildBooking bArgs searchReq driverQuote searchTry.billingCategory driverQuote.id.getId driverQuote.tripCategory now Nothing Nothing (Just driverQuote.distanceToPickup) Nothing searchReq.configInExperimentVersions driverQuote.coinsRewardedOnGoldTierRide driverQuote.preferenceMatchScore (Just driverQuote.searchTryId) (Just driverQuote.durationToPickup) searchTry.emailDomain searchTry.businessEmailDomain driverQuote.isAutoAccepted searchTry.addOnData
    triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = merchant.id}
    QRB.createBooking booking
    cityLabel <- SML.getCityLabel booking.merchantOperatingCityId
    metricsDistanceBucketEdges <- SML.getDistanceBucketEdges booking.merchantOperatingCityId
    let (pickupZone, dropZone) = SML.specialZoneLabels booking.area
    Metrics.incrementBookingCreatedCount merchant.shortId.getShortId cityLabel (show booking.vehicleServiceTier) (SML.distanceBucketLabel metricsDistanceBucketEdges booking.estimatedDistance) pickupZone dropZone
    -- Assignment gate: any other driver responding after this gets RideRequestAlreadyAccepted.
    QST.updateStatus DST.COMPLETED searchTry.id
    postBookingResult <- withTryCatch "betterDriverSwapAssign:postBooking" $ do
      mFleetAssociation <- QFDA.findByDriverId driver.id True
      (ride, rideDetails, vehicle) <- initializeRide merchant driver booking Nothing Nothing clientId Nothing (mFleetAssociation <&> (.fleetOwnerId) <&> Id) True True Nothing
      void $ deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id driverQuote.searchTryId (mkPrice (Just driverQuote.currency) driverQuote.estimatedFare) (Just transporterConfig)
      let prefetch =
            BP.RideAssignedPrefetch
              { merchant = Just merchant,
                driverInfo = Just driverInfo,
                driverStats = Just driverStats,
                transporterConfig = Just transporterConfig,
                rideDetails = Just rideDetails
              }
      -- NOT forked: the old ride is only cancelled once the BAP accepts the replacement.
      -- Cancelling first and failing here would leave the rider with no ride at all.
      payload <- (\p -> p {CallBAPInternal.oldBppBookingId = Just oldBooking.id.getId}) <$> buildOneShotAssignPayload prefetch booking ride driver vehicle driverQuote
      appBackendBapInternal <- asks (.appBackendBapInternal)
      void $ withQuickRetry $ CallBAPInternal.oneShotAssign appBackendBapInternal.apiKey appBackendBapInternal.url payload
      logInfo $ "Better-driver swap: BAP accepted replacement booking " <> booking.id.getId <> " for old booking " <> oldBookingIdText
      -- BAP is now pointing the rider at the new booking, so retire the old ride.
      cancelOldRide oldBooking oldRide
      rideResResult <- withTryCatch "betterDriverSwapAssign:driverRideRes" $ buildAssignedDriverRideRes driver driverInfo transporterConfig booking ride rideDetails
      case rideResResult of
        Right res -> pure (Just res)
        Left err -> do
          logError $ "Better-driver swap: building driver ride response failed for ride " <> ride.id.getId <> ": " <> show err
          pure Nothing
    case postBookingResult of
      Right mbRideRes -> pure mbRideRes
      Left err -> do
        -- Rollback: cancel the replacement, leave the rider with their original driver.
        -- No search-expired signal - the rider never knew a search was running.
        logError $ "Better-driver swap failed after booking creation for " <> booking.id.getId <> ", cancelling replacement: " <> show err
        cancelResult <- withTryCatch "betterDriverSwapCancelBooking" $ cancelBooking booking (Just driver) merchant
        case cancelResult of
          Left cancelErr -> logError $ "Better-driver swap: cancelling replacement booking " <> booking.id.getId <> " failed: " <> show cancelErr
          Right _ -> pure ()
        throwM err
  Redis.unlockRedis (mkCancelSearchInitLockKey searchReq.transactionId)
  either throwM pure result
  where
    cancelOldRide oldBooking oldRide = do
      cancelledAt <- getCurrentTime
      let bookingCReason =
            SBCR.BookingCancellationReason
              { bookingId = oldBooking.id,
                rideId = Just oldRide.id,
                merchantId = Just merchant.id,
                source = SBCR.ByAllocator,
                reasonCode = Just (DTCR.CancellationReasonCode "RIDER_FOUND_BETTER_MATCH"),
                driverId = Just oldRide.driverId,
                additionalInfo = Just "Rider found a better driver match",
                ondcCancellationReasonId = Nothing,
                driverCancellationLocation = Nothing,
                driverDistToPickup = Nothing,
                distanceUnit = oldBooking.distanceUnit,
                merchantOperatingCityId = Just oldBooking.merchantOperatingCityId,
                createdAt = Just cancelledAt,
                updatedAt = Just cancelledAt
              }
      QBCR.upsert bookingCReason
      QRide.updateStatus oldRide.id DRide.CANCELLED
      -- Booking goes with the ride, else the rider ends up holding two live bookings.
      QRB.updateStatus oldBooking.id DRB.CANCELLED
      logInfo $ "Better-driver swap: cancelled old ride " <> oldRide.id.getId <> " and booking " <> oldBooking.id.getId
      -- Best-effort: the swap is already committed and the rider already points at the new booking.
      fork "better-driver-swap cancel old booking on BAP" $ do
        cancelUpdateResult <-
          withTryCatch "betterDriverSwapCancelOldBookingOnBAP" $
            BP.sendBookingCancelledUpdateToBAP oldBooking merchant SBCR.ByAllocator Nothing Nothing (Just oldRide)
        case cancelUpdateResult of
          Right _ -> logInfo $ "Better-driver swap: BAP told to cancel old booking " <> oldBooking.id.getId
          Left err -> logError $ "Better-driver swap: telling BAP to cancel old booking " <> oldBooking.id.getId <> " failed: " <> show err
