module SharedLogic.NudgeRide
  ( NudgePayoutConfig (..),
    nudgeRideRedisKey,
    nudgeRidePayoutCfgKey,
    nudgeRideCleanupDoneKey,
    nudgeRideTtlSeconds,
    getPayoutConfigFromCohort,
    createNudgeBookingAndRide,
  )
where

import qualified Data.Aeson as Ae
import Data.Aeson.Types (parseMaybe)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FarePolicy.PlatformFeeMethods as FPM
import qualified Domain.Types.Location as DL
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DTMO
import Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as IJCommon
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
import qualified Lib.IncentiveJourney.Storage.Queries.CohortJourneyMapping as QCJM
import qualified Lib.IncentiveJourney.Storage.Queries.UserCohortMappingExtra as QUCMExtra
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.Allocator (AllocatorJobType (..), CompleteStaleNudgeRideJobData (..))
import qualified SharedLogic.Ride as SLRide
import qualified SharedLogic.Type as SLT
import Storage.Beam.IncentiveJourney ()
import qualified Storage.Queries.BookingExtra as QBooking
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideD
import qualified Tools.Notifications as TN

data NudgePayoutConfig = NudgePayoutConfig
  { payoutAmountPerKm :: HighPrecMoney,
    bufferDistanceKm :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

nudgeRideRedisKey :: Id Person -> Text
nudgeRideRedisKey driverId = "nudgeRide:" <> driverId.getId

nudgeRidePayoutCfgKey :: Id DRide.Ride -> Text
nudgeRidePayoutCfgKey rideId = "nudgeRidePayoutCfg:" <> rideId.getId

nudgeRideCleanupDoneKey :: Id DRide.Ride -> Text
nudgeRideCleanupDoneKey rideId = "nudgeRideCleanupDone:" <> rideId.getId

nudgeRideTtlSeconds :: Int
nudgeRideTtlSeconds = 2 * 60 * 60

getPayoutConfigFromCohort ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  m (Maybe NudgePayoutConfig)
getPayoutConfigFromCohort driverId = do
  let personId = cast driverId :: Id IJCommon.Person
  ucms <- QUCMExtra.findActiveByUserId personId
  mappings <- mapM (\(ucm :: DUCM.UserCohortMapping) -> QCJM.findById ucm.cohortMappingId) ucms
  pure $ listToMaybe $ mapMaybe parseConfig mappings
  where
    parseConfig (Just (m :: DCJM.CohortJourneyMapping)) = do
      v <- m.streakEndRewardMetadata
      parseMaybe
        ( Ae.withObject "NudgePayoutConfig" $ \obj ->
            NudgePayoutConfig
              <$> obj Ae..: "payoutAmountPerKm"
              <*> obj Ae..: "bufferDistanceKm"
        )
        v
    parseConfig Nothing = Nothing

createNudgeBookingAndRide ::
  Merchant ->
  Person ->
  DVeh.Vehicle ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  LatLong ->
  DTMO.Overlay ->
  Flow (DBooking.Booking, DRide.Ride)
createNudgeBookingAndRide merchant driver vehicle merchantOpCityId driverLoc hotspotLoc overlay = do
  now <- getCurrentTime
  bookingId <- Id <$> generateGUID
  fareParamsId <- Id <$> generateGUID
  transactionId <- generateGUID
  quoteId <- generateGUID
  fromLocId <- Id <$> generateGUID
  toLocId <- Id <$> generateGUID

  -- Payout config
  mbPayoutCfg <- getPayoutConfigFromCohort driver.id
  let estimatedDistMeters = highPrecMetersToMeters (distanceBetweenInMeters driverLoc hotspotLoc)
      routeDistanceKm = fromIntegral estimatedDistMeters.getMeters / 1000.0 :: Double
      estimatedFare = case mbPayoutCfg of
        Just cfg ->
          realToFrac (max 0.0 (routeDistanceKm - cfg.bufferDistanceKm)) * cfg.payoutAmountPerKm
        Nothing -> 0

  -- Build FareParameters
  let fareParams =
        DFP.FareParameters
          { id = fareParamsId,
            driverSelectedFare = Nothing,
            customerExtraFee = Nothing,
            negativeFareAdjustment = Nothing,
            serviceCharge = Nothing,
            parkingCharge = Nothing,
            stopCharges = Nothing,
            govtCharges = Nothing,
            baseFare = estimatedFare,
            waitingCharge = Nothing,
            rideExtraTimeFare = Nothing,
            nightShiftCharge = Nothing,
            nightShiftRateIfApplies = Nothing,
            fareParametersDetails =
              DFP.ProgressiveDetails $
                DFP.FParamsProgressiveDetails
                  { deadKmFare = 0,
                    extraKmFare = Nothing,
                    rideDurationFare = Nothing,
                    currency = INR
                  },
            tollCharges = Nothing,
            congestionCharge = Nothing,
            petCharges = Nothing,
            driverAllowance = Nothing,
            airportConvenienceFee = Nothing,
            businessDiscount = Nothing,
            personalDiscount = Nothing,
            priorityCharges = Nothing,
            congestionChargeViaDp = Nothing,
            insuranceCharge = Nothing,
            cardCharge = Nothing,
            luggageCharge = Nothing,
            returnFeeCharge = Nothing,
            boothCharge = Nothing,
            schedulingCharge = Nothing,
            platformFee = Nothing,
            sgst = Nothing,
            cgst = Nothing,
            tdsAmount = Nothing,
            tdsRate = Nothing,
            tdsProcessedAt = Nothing,
            platformFeeChargesBy = FPM.NoCharge,
            currency = INR,
            updatedAt = now,
            merchantId = Just merchant.id,
            merchantOperatingCityId = Just merchantOpCityId,
            conditionalCharges = [],
            customerGateFeeItems = Nothing,
            shouldApplyBusinessDiscount = False,
            shouldApplyPersonalDiscount = False,
            driverCancellationNotAllowed = Nothing,
            paymentProcessingFee = Nothing,
            paymentProcessingFeeTax = Nothing,
            isVatTaxType = Nothing,
            discountApplicableRideFareTaxExclusive = Nothing,
            discountApplicableRideFareTax = Nothing,
            nonDiscountApplicableRideFareTaxExclusive = Nothing,
            nonDiscountApplicableRideFareTax = Nothing,
            tollFareTaxExclusive = Nothing,
            tollFareTax = Nothing,
            cancellationFeeTaxExclusive = Nothing,
            cancellationTax = Nothing,
            parkingChargeTaxExclusive = Nothing,
            parkingChargeTax = Nothing,
            fareSettlementType = Nothing,
            negotiatedFareDelta = Nothing,
            bufferedFare = Nothing,
            addOnCharges = Nothing,
            rideTaxCgst = Nothing,
            rideTaxSgst = Nothing,
            rideTaxIgst = Nothing,
            parkingTaxCgst = Nothing,
            parkingTaxSgst = Nothing,
            parkingTaxIgst = Nothing,
            tollTaxCgst = Nothing,
            tollTaxSgst = Nothing,
            tollTaxIgst = Nothing,
            airportEntryFee = Nothing,
            airportEntryFeeTax = Nothing,
            airportEntryFeeTaxCgst = Nothing,
            airportEntryFeeTaxSgst = Nothing,
            airportEntryFeeTaxIgst = Nothing,
            paymentChargeTaxCgst = Nothing,
            paymentChargeTaxSgst = Nothing,
            paymentChargeTaxIgst = Nothing,
            customerCancellationDues = Nothing,
            cancellationDuesTax = Nothing,
            cancellationDuesTaxCgst = Nothing,
            cancellationDuesTaxSgst = Nothing,
            cancellationDuesTaxIgst = Nothing
          }

  let emptyAddr =
        DL.LocationAddress
          { area = Nothing,
            areaCode = Nothing,
            building = Nothing,
            city = Nothing,
            country = Nothing,
            door = Nothing,
            extras = Nothing,
            fullAddress = Nothing,
            instructions = Nothing,
            state = Nothing,
            street = Nothing
          }

  let fromLocation =
        DL.Location
          { id = fromLocId,
            lat = driverLoc.lat,
            lon = driverLoc.lon,
            address = emptyAddr,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchant.id,
            merchantOperatingCityId = Just merchantOpCityId
          }
      toLocation =
        DL.Location
          { id = toLocId,
            lat = hotspotLoc.lat,
            lon = hotspotLoc.lon,
            address = emptyAddr,
            createdAt = now,
            updatedAt = now,
            merchantId = Just merchant.id,
            merchantOperatingCityId = Just merchantOpCityId
          }

  let vehicleServiceTier = fromMaybe DTC.AUTO_RICKSHAW (listToMaybe vehicle.selectedServiceTiers)
      bapId = merchant.subscriberId.getShortId
      booking =
        DBooking.Booking
          { id = bookingId,
            transactionId = transactionId,
            quoteId = quoteId,
            status = DBooking.NEW,
            hasStops = Nothing,
            bookingDeposit = Nothing,
            dynamicPricingLogicVersion = Nothing,
            estimatedCongestionCharge = Nothing,
            tripCategory = DTC.OneWay DTC.OneWayRideOtp,
            specialZoneOtpCode = Nothing,
            specialLocationTag = Nothing,
            specialLocationName = Nothing,
            fareSettlementType = Nothing,
            disabilityTag = Nothing,
            area = Nothing,
            pickupGateId = Nothing,
            providerId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            fromLocGeohash = Nothing,
            toLocGeohash = Nothing,
            primaryExophone = "NudgeRide",
            bapId = bapId,
            billingCategory = SLT.PERSONAL,
            bapUri = bapId,
            bapCity = Nothing,
            bapCountry = Nothing,
            startTime = now,
            riderId = Nothing,
            numberShareConsent = Nothing,
            fromLocation = fromLocation,
            toLocation = Just toLocation,
            stops = [],
            vehicleServiceTier = vehicleServiceTier,
            vehicleServiceTierName = show vehicleServiceTier,
            vehicleServiceTierSeatingCapacity = Nothing,
            vehicleServiceTierAirConditioned = Nothing,
            isAirConditioned = Nothing,
            estimatedDistance = Just estimatedDistMeters,
            maxEstimatedDistance = Nothing,
            estimatedFare = estimatedFare,
            currency = INR,
            estimatedDuration = Nothing,
            estimatedStaticDuration = Nothing,
            fareParams = fareParams,
            riderName = Nothing,
            paymentMethodId = Nothing,
            tollCharges = Nothing,
            tollNames = Nothing,
            tollIds = Nothing,
            createdAt = now,
            updatedAt = now,
            paymentUrl = Nothing,
            stopLocationId = Nothing,
            distanceToPickup = Nothing,
            distanceUnit = Meter,
            returnTime = Nothing,
            roundTrip = Nothing,
            isScheduled = False,
            estimateId = Nothing,
            paymentId = Nothing,
            isDashboardRequest = False,
            bookingType = DBooking.NudgeRide,
            senderDetails = Nothing,
            receiverDetails = Nothing,
            parcelType = Nothing,
            parcelQuantity = Nothing,
            initiatedAs = Nothing,
            isReferredRide = Nothing,
            isSafetyPlus = False,
            isPetRide = False,
            preferenceMatchScore = Nothing,
            configInExperimentVersions = [],
            coinsRewardedOnGoldTierRide = Nothing,
            isAutoAccepted = Nothing,
            isInsured = False,
            insuredAmount = Nothing,
            exotelDeclinedCallStatusReceivingTime = Nothing,
            numberOfLuggages = Nothing,
            numberOfCabinBags = Nothing,
            numberOfCheckInBags = Nothing,
            numberOfPassengers = Nothing,
            isPickupOrDestinationEdited = Nothing,
            paymentInstrument = Nothing,
            paymentMode = Nothing,
            commission = Nothing,
            cancellationDuesCommission = Nothing,
            cancellationDuesCommissionTaxBreakup = Nothing,
            paymentCharge = Nothing,
            paymentChargeBearer = Nothing,
            discountAmount = Nothing,
            displayBookingId = Nothing,
            searchTryId = Nothing,
            dqDurationToPickup = Nothing,
            reconciliationStatus = Nothing,
            ledgerWriteMode = Nothing,
            financeInvoiceId = Nothing,
            emailDomain = Nothing,
            businessEmailDomain = Nothing,
            customerLanguage = Nothing,
            addOnData = [],
            paymentChargeDetails = Nothing,
            taxRates = Nothing
          }

  QFP.create fareParams
  QBooking.createBooking booking

  otp <- generateOTPCode
  ride <- SLRide.buildRide driver booking Nothing otp Nothing Nothing Nothing now vehicle False (Just True) Nothing Nothing Nothing Nothing Nothing Nothing
  rideDetails <- SLRide.buildRideDetails booking ride driver vehicle
  QRide.createRide ride
  QRideD.create rideDetails

  -- Store payout config in Redis for use at ride end
  whenJust mbPayoutCfg $ \cfg ->
    Redis.setExp (nudgeRidePayoutCfgKey ride.id) cfg nudgeRideTtlSeconds

  -- Schedule stale-ride cleanup job for 2 hours from now
  let cleanupTime = addUTCTime (fromIntegral nudgeRideTtlSeconds) now
  QAllJ.createJobByTime @_ @'CompleteStaleNudgeRide (Just merchant.id) (Just merchantOpCityId) cleanupTime $
    CompleteStaleNudgeRideJobData {rideId = ride.id, driverId = driver.id}

  -- Send overlay notification to driver
  TN.sendDemandNudgeOverlay merchantOpCityId driver (TN.mkOverlayReq overlay) $
    TN.DemandNudgeEntityData
      { location = hotspotLoc,
        nudgeId = ride.id.getId
      }

  pure (booking, ride)
