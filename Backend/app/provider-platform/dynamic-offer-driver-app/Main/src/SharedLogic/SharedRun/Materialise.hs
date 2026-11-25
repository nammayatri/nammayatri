{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The shared-run materialiser: turns a city's GTFS timetables into scheduled runs (R1) and hands each new run to the
-- vehicle bound to its route (R2).
--
-- A run is a Booking with trip category SharedRoute, built the way a Beckn init would build one but from a GTFS trip:
-- the trip's stops are its pickup, stops and drop, its first departure is the start time, the city's SharedRoute
-- fare_product prices it (the run's real fare is the sum of its tickets at run end). One SearchRequest and one Quote
-- stand behind it, because the scheduled-ride accept path reads the quote. The booking goes on the scheduled board;
-- when vehicle_route_mapping binds a vehicle to the route, that vehicle's driver is assigned straight away, as a fleet
-- owner would from the dashboard.
--
-- Idempotent: a run is keyed by feed, trip and service day in its transaction id, and a day that already has the run
-- (whatever its status) is left alone. Polymorphic in its monad so the allocator's MaterialiseSharedRuns job runs it
-- in-process once a day per city; the assignment is the scheduled accept's own steps (SharedLogic.Ride.initializeRide).
module SharedLogic.SharedRun.Materialise
  ( MaterialiseFlow,
    MaterialiseReq (..),
    MaterialiseResp (..),
    DayResult (..),
    materialiseRuns,
    runTransactionId,
    sharedRunBapId,
  )
where

import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..), addDays)
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types as DTrip
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FareProduct as DFareProduct
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RiderPreferredOption as DRPO
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SharedRoute (ScheduleMode (..), SharedSupplyKnobs (..))
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.Vehicle as Fields
import qualified Domain.Types.VehicleRouteMapping as Fields
import qualified Domain.Types.VehicleServiceTier as Fields
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.GtfsDataServer.Flow as Gtfs
import Lib.GtfsDataServer.Types (GtfsTimetablePattern (..), GtfsTimetableRoute (..), GtfsTimetableStop (..), GtfsTimetableStopTime (..), GtfsTimetableTrip (..), gtfsLocalId)
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.FareCalculator as FC
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.Ride as SRide
import qualified SharedLogic.SharedRun.Assign as Assign
import qualified SharedLogic.SharedRun.Coverage as Coverage
import qualified SharedLogic.SharedRun.Knobs as Knobs
import qualified SharedLogic.Type as SLT
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.FareProduct as QFareProduct
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRouteMapping as QVRM

-- | What the materialiser asks of its environment: what creating a ride asks (the assignment), plus the fare policy's
-- ClickHouse read and the timetable's and own-address lookups. The driver app's Flow and the allocator's both have it.
type MaterialiseFlow m r c =
  ( SRide.InitializeRideFlow m r c,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  )

data MaterialiseReq = MaterialiseReq
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    -- | one service day; Nothing: today and the next 'lookaheadDays' days (local to the city)
    serviceDate :: Maybe Day,
    lookaheadDays :: Maybe Int
  }
  deriving (Generic, Show)

newtype MaterialiseResp = MaterialiseResp {days :: [DayResult]}
  deriving (Generic, Show)

data DayResult = DayResult
  { serviceDate :: Day,
    feeds :: Int,
    trips :: Int,
    created :: Int,
    existing :: Int,
    assigned :: Int,
    unassigned :: Int,
    -- | one line per feed or trip that could not be materialised; the others went through
    failures :: [Text]
  }
  deriving (Generic, Show)

-- | The run's own "BAP": there is none, so the id says so wherever a booking's BAP is read.
sharedRunBapId :: Text
sharedRunBapId = "shared-run"

-- | The idempotency key of one run: the feed's config, the GTFS trip and the service day.
runTransactionId :: Id DIBC.IntegratedBPPConfig -> Text -> Day -> Text
runTransactionId integratedBppConfigId tripId day = "sharedRun:" <> integratedBppConfigId.getId <> ":" <> tripId <> ":" <> show day

materialiseRuns :: MaterialiseFlow m r c => DM.Merchant -> MaterialiseReq -> m MaterialiseResp
materialiseRuns merchant req = do
  moc <- CQMOC.findById req.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound req.merchantOperatingCityId.getId)
  unless (moc.merchantId == merchant.id) $ throwError (InvalidRequest "the city does not belong to the merchant")
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = moc.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound moc.id.getId)
  knobs <- Knobs.requireSupplyKnobs transporterConfig
  when (knobs.schedule /= TIMETABLED) $ Knobs.notImplemented ("materialising " <> show knobs.schedule <> " runs: only TIMETABLED feeds are read (module R)")
  localNow <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let today = utctDay localNow
      days = case req.serviceDate of
        Just day -> [day]
        Nothing -> [addDays (toInteger d) today | d <- [0 .. max 0 (fromMaybe 1 req.lookaheadDays)]]
  fareProduct <- runFareProduct moc.id
  MaterialiseResp <$> forM days (materialiseDay merchant moc transporterConfig knobs fareProduct)

-- | The city's run fare product: the one enabled SharedRoute row, which names the service tier of a run and the fare
-- policy whose commission config PER_TICKET applies at run end.
runFareProduct :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m DFareProduct.FareProduct
runFareProduct cityId = do
  products <- QFareProduct.findAllFareProductByMerchantOpCityId cityId True
  find (DTrip.isSharedRouteTrip . (.tripCategory)) products
    & fromMaybeM (InvalidRequest $ "No enabled SharedRoute fare_product in city " <> cityId.getId <> "; it names the service tier and fare policy of a run")

data Outcome = Existing | Created Assign.Assignment

materialiseDay :: MaterialiseFlow m r c => DM.Merchant -> DMOC.MerchantOperatingCity -> DTC.TransporterConfig -> SharedSupplyKnobs -> DFareProduct.FareProduct -> Day -> m DayResult
materialiseDay merchant moc transporterConfig knobs fareProduct day = do
  perFeed <- forM knobs.feeds $ \feedKey -> do
    configs <- QIBC.findAllByFeedKeyAndCity feedKey moc.id
    case listToMaybe configs of
      Nothing -> pure (Left ("feed " <> feedKey <> " has no integrated bpp config in city " <> moc.id.getId), [])
      Just integratedBppConfig -> do
        timetable <- withTryCatch "sharedRun:timetable" $ do
          baseUrl <- SIBC.getGimsBaseUrl integratedBppConfig
          Gtfs.getFeedTimetableForDate baseUrl integratedBppConfig.feedKey day
        case timetable of
          Left err -> pure (Left ("feed " <> feedKey <> ": timetable for " <> show day <> " could not be read: " <> show err), [])
          Right routes -> do
            outcomes <- forM [(route, trip) | route <- routes, pat <- route.patterns, trip <- pat.tripsForDate] $ \(route, trip) -> do
              res <- withTryCatch "sharedRun:materialiseTrip" $ materialiseTrip merchant moc transporterConfig fareProduct integratedBppConfig day route trip
              pure $ either (\err -> Left ("trip " <> gtfsLocalId trip.gtfsId <> " of " <> gtfsLocalId route.gtfsId <> ": " <> show err)) Right res
            pure (Right (), outcomes)
  let feedFailures = [msg | (Left msg, _) <- perFeed]
      outcomes = concatMap snd perFeed
      result =
        DayResult
          { serviceDate = day,
            feeds = length knobs.feeds,
            trips = length outcomes,
            created = length [() | Right (Created _) <- outcomes],
            existing = length [() | Right Existing <- outcomes],
            assigned = length [() | Right (Created (Assign.Assigned _)) <- outcomes],
            unassigned = length [() | Right (Created (Assign.Unassigned _)) <- outcomes],
            failures = feedFailures <> [msg | Left msg <- outcomes] <> ["unassigned: " <> why | Right (Created (Assign.Unassigned why)) <- outcomes]
          }
  logInfo $ "shared runs for " <> show day <> " in city " <> moc.id.getId <> ": " <> show result
  pure result

materialiseTrip :: MaterialiseFlow m r c => DM.Merchant -> DMOC.MerchantOperatingCity -> DTC.TransporterConfig -> DFareProduct.FareProduct -> DIBC.IntegratedBPPConfig -> Day -> GtfsTimetableRoute -> GtfsTimetableTrip -> m Outcome
materialiseTrip merchant moc transporterConfig fareProduct integratedBppConfig day route trip = do
  let tripId = gtfsLocalId trip.gtfsId
      routeCode = gtfsLocalId route.gtfsId
      transactionId = runTransactionId integratedBppConfig.id tripId day
  stopTimes <- NE.nonEmpty (sortOn (.stopPosition) trip.stoptimes) & fromMaybeM (InternalError $ "trip " <> tripId <> " has no stop times")
  QRB.findByTransactionId transactionId >>= \case
    Just _ -> pure Existing
    Nothing -> do
      now <- getCurrentTime
      -- GTFS times count from the service day's local midnight
      let dayStart = addUTCTime (negate (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc)) (UTCTime day 0)
          firstStop = NE.head stopTimes
          lastStop = NE.last stopTimes
          startTime = addUTCTime (fromIntegral firstStop.scheduledDeparture) dayStart
          endTime = addUTCTime (fromIntegral lastStop.scheduledArrival) dayStart
          duration = Seconds (max 60 (lastStop.scheduledArrival - firstStop.scheduledDeparture))
          tripCategory = fareProduct.tripCategory
      locations <- forM stopTimes (stopLocation merchant moc now)
      when (length locations < 2) $ throwError (InternalError $ "trip " <> tripId <> " has fewer than two stops")
      let fromLocation = NE.head locations
          toLocation = NE.last locations
          between = drop 1 (NE.init locations)
      let distance = Meters . roundToIntegral . sum $ zipWith (\a b -> distanceBetweenInMeters (LatLong a.lat a.lon) (LatLong b.lat b.lon)) (NE.toList locations) (NE.tail locations)
          fromPoint = LatLong fromLocation.lat fromLocation.lon
          toPoint = LatLong toLocation.lat toLocation.lon
      -- priced like a static-offer quote off the run fare product; the fare at run end is the sum of the tickets
      fullFarePolicy <- SFP.getFarePolicy Nothing (Just fromPoint) (Just toPoint) Nothing Nothing (Just distance) (Just duration) moc.id False tripCategory fareProduct.vehicleServiceTier Nothing (Just startTime) Nothing Nothing [] Nothing
      fareParams <- SFP.calculateFareParametersForFarePolicy fullFarePolicy (Just distance) (Just duration) moc.id
      let estimatedFare = FC.fareSum fareParams Nothing
      mbTier <- CQVST.findByServiceTierTypeAndCityId fareProduct.vehicleServiceTier moc.id Nothing
      let tierName = maybe (show fareProduct.vehicleServiceTier) (.name) mbTier
      searchRequestId <- generateGUID
      quoteId <- generateGUID
      bookingId <- generateGUID
      nwAddress <- asks (.nwAddress)
      let searchRequest =
            DSR.SearchRequest
              { id = searchRequestId,
                transactionId,
                messageId = Nothing,
                providerId = merchant.id,
                merchantOperatingCityId = moc.id,
                bapId = sharedRunBapId,
                bapUri = nwAddress,
                bapCity = Just moc.city,
                bapCountry = Just moc.country,
                area = Just SL.Default,
                pickupArea = Nothing,
                dropArea = Nothing,
                autoAssignEnabled = Nothing,
                fromLocation,
                toLocation = Just toLocation,
                stops = between,
                hasStops = Just (not (null between)),
                fromLocGeohash = Nothing,
                toLocGeohash = Nothing,
                estimatedDistance = Just distance,
                estimatedDuration = Just duration,
                estimatedStaticDuration = Just duration,
                startTime,
                validTill = endTime,
                returnTime = Nothing,
                roundTrip = Nothing,
                isScheduled = True,
                isAdvanceBookingEnabled = False,
                isReallocationEnabled = Just False,
                isSilentReallocationEnabled = Nothing,
                silentReallocationWindowSeconds = Nothing,
                isReserveRide = Nothing,
                isDashboardRequest = False,
                isPetRide = False,
                preferSafetyPlus = False,
                isBlockedRoute = Nothing,
                isCustomerPrefferedSearchRoute = Nothing,
                riderId = Nothing,
                riderName = Nothing,
                riderPreferredOption = DRPO.OneWay,
                tripCategory = Just tripCategory,
                customerLanguage = Nothing,
                customerNammaTags = Nothing,
                customerCancellationDues = Nothing,
                customerCancellationDuesTaxBreakup = Nothing,
                customerCancellationDuesCommission = Nothing,
                customerCancellationDuesCommissionTaxBreakup = Nothing,
                device = Nothing,
                cloudType = Nothing,
                disabilityTag = Nothing,
                driverDefaultExtraFee = Nothing,
                driverIdForSearch = Nothing,
                dynamicPricingLogicVersion = Nothing,
                poolingLogicVersion = Nothing,
                poolingConfigVersion = Nothing,
                configInExperimentVersions = [],
                searchTags = Nothing,
                specialLocationName = Nothing,
                specialLocationTag = Nothing,
                pickupGateId = Nothing,
                pickupZoneGateId = Nothing,
                tollCharges = Nothing,
                tollIds = Nothing,
                tollNames = Nothing,
                parcelType = Nothing,
                parcelQuantity = Nothing,
                paymentMode = Nothing,
                numberOfLuggages = Nothing,
                numberOfCheckInBags = Nothing,
                numberOfCabinBags = Nothing,
                numberOfPassengers = Nothing,
                userBackendAppVersion = Nothing,
                userBundleVersion = Nothing,
                userClientDevice = Nothing,
                userSdkVersion = Nothing,
                currency = moc.currency,
                distanceUnit = moc.distanceUnit,
                createdAt = now
              }
      exophone <- DInit.findRandomExophone moc.id searchRequest DExophone.CALL_RIDE
      let quote =
            DQuote.Quote
              { id = quoteId,
                searchRequestId,
                providerId = merchant.id,
                vehicleServiceTier = fareProduct.vehicleServiceTier,
                vehicleServiceTierName = Just tierName,
                estimatedFinishTime = Just endTime,
                tripCategory,
                validTill = endTime,
                estimatedFare,
                currency = moc.currency,
                distance = Just distance,
                distanceUnit = moc.distanceUnit,
                specialLocationTag = Nothing,
                fareParams,
                farePolicy = Just (DFP.fullFarePolicyToFarePolicy fullFarePolicy),
                isScheduled = True,
                isCustomerPrefferedSearchRoute = Nothing,
                isBlockedRoute = Nothing,
                tollNames = Nothing,
                area = Nothing,
                navigationInstruction = Nothing,
                createdAt = now,
                updatedAt = now,
                merchantOperatingCityId = Just moc.id,
                addOnData = [],
                isDerived = False
              }
          booking =
            DRB.Booking
              { id = bookingId,
                transactionId,
                quoteId = quoteId.getId,
                status = DRB.NEW,
                tripCategory,
                tripId = Just tripId,
                routeCode = Just routeCode,
                integratedBppConfigId = Just integratedBppConfig.id.getId,
                providerId = merchant.id,
                merchantOperatingCityId = moc.id,
                primaryExophone = exophone.primaryPhone,
                bapId = sharedRunBapId,
                bapUri = showBaseUrl nwAddress,
                bapCity = Just moc.city,
                bapCountry = Just moc.country,
                billingCategory = SLT.PERSONAL,
                startTime,
                returnTime = Nothing,
                roundTrip = Nothing,
                isScheduled = True,
                fromLocation,
                toLocation = Just toLocation,
                stops = between,
                hasStops = Just (not (null between)),
                stopLocationId = Just toLocation.id,
                fromLocGeohash = Nothing,
                toLocGeohash = Nothing,
                vehicleServiceTier = fareProduct.vehicleServiceTier,
                vehicleServiceTierName = tierName,
                vehicleServiceTierSeatingCapacity = mbTier >>= (.seatingCapacity),
                vehicleServiceTierAirConditioned = mbTier >>= (.airConditionedThreshold),
                isAirConditioned = mbTier >>= (.isAirConditioned),
                estimatedDistance = Just distance,
                maxEstimatedDistance = Nothing,
                estimatedDuration = Just duration,
                estimatedStaticDuration = Just duration,
                estimatedFare,
                estimatedCongestionCharge = Nothing,
                fareParams,
                currency = moc.currency,
                distanceUnit = moc.distanceUnit,
                area = Just SL.Default,
                riderId = Nothing,
                riderName = Nothing,
                riderCallIdentity = Nothing,
                numberShareConsent = Nothing,
                customerLanguage = Nothing,
                disabilityTag = Nothing,
                specialLocationTag = Nothing,
                specialLocationName = Nothing,
                specialZoneOtpCode = Nothing,
                pickupGateId = Nothing,
                fareSettlementType = Nothing,
                bookingDeposit = Nothing,
                dynamicPricingLogicVersion = Nothing,
                configInExperimentVersions = [],
                isDashboardRequest = False,
                isPetRide = False,
                isInsured = False,
                insuredAmount = Nothing,
                isSafetyPlus = False,
                isReferredRide = Nothing,
                isAutoAccepted = Nothing,
                isPickupOrDestinationEdited = Just False,
                preferenceMatchScore = Nothing,
                coinsRewardedOnGoldTierRide = Nothing,
                commission = Nothing,
                cancellationDuesCommission = Nothing,
                cancellationDuesCommissionTaxBreakup = Nothing,
                discountAmount = Nothing,
                taxRates = Nothing,
                paymentMethodId = Nothing,
                paymentInstrument = Nothing,
                paymentMode = Nothing,
                paymentUrl = Nothing,
                paymentId = Nothing,
                paymentCharge = Nothing,
                paymentChargeBearer = Nothing,
                paymentChargeDetails = Nothing,
                tollCharges = Nothing,
                tollNames = Nothing,
                tollIds = Nothing,
                estimateId = Nothing,
                searchTryId = Nothing,
                distanceToPickup = Nothing,
                dqDurationToPickup = Nothing,
                displayBookingId = Nothing,
                emailDomain = Nothing,
                businessEmailDomain = Nothing,
                initiatedAs = Nothing,
                senderDetails = Nothing,
                receiverDetails = Nothing,
                parcelType = Nothing,
                parcelQuantity = Nothing,
                numberOfLuggages = Nothing,
                numberOfCheckInBags = Nothing,
                numberOfCabinBags = Nothing,
                numberOfPassengers = Nothing,
                addOnData = [],
                exotelDeclinedCallStatusReceivingTime = Nothing,
                reconciliationStatus = Nothing,
                ledgerWriteMode = Nothing,
                financeInvoiceId = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QSR.createDSReq searchRequest
      QQuote.create quote
      -- the accept path and run end read the policy (commission config) by quote id, as for any scheduled ride
      SFP.cacheFarePolicyByScheduledEstOrQuoteId quoteId.getId startTime Nothing fullFarePolicy
      QRB.createBooking booking
      SBooking.addScheduledBookingInRedis booking
      Coverage.scheduleCoverageJobs transporterConfig booking
      logInfo $ "shared run created: booking " <> bookingId.getId <> " for trip " <> tripId <> " of " <> routeCode <> " at " <> show startTime
      Created <$> assignBoundVehicle merchant transporterConfig booking routeCode

-- | A GTFS stop as a Location row of the run. The address carries the stop's name and, in @extras@, its code.
stopLocation :: MonadFlow m => DM.Merchant -> DMOC.MerchantOperatingCity -> UTCTime -> GtfsTimetableStopTime -> m DLoc.Location
stopLocation merchant moc now stopTime = do
  locationId <- generateGUID
  let stop = stopTime.stop
      stopCode = fromMaybe (gtfsLocalId stop.gtfsId) stop.code
  pure
    DLoc.Location
      { id = locationId,
        lat = stop.lat,
        lon = stop.lon,
        address =
          DLoc.LocationAddress
            { street = Nothing,
              door = Nothing,
              city = Just (show moc.city),
              state = Nothing,
              country = Just (show moc.country),
              building = Nothing,
              areaCode = Nothing,
              area = stop.name,
              fullAddress = stop.name,
              instructions = Nothing,
              extras = Just stopCode
            },
        merchantId = Just merchant.id,
        merchantOperatingCityId = Just moc.id,
        createdAt = now,
        updatedAt = now
      }

-- | The run goes to the driver of the first vehicle bound to the route that can take it
-- ('Assign.assignFirstAvailable', the scheduled accept's own steps). A failed assignment leaves the run on the
-- scheduled board for the next driver or the fallback job, and says why.
assignBoundVehicle :: MaterialiseFlow m r c => DM.Merchant -> DTC.TransporterConfig -> DRB.Booking -> Text -> m Assign.Assignment
assignBoundVehicle merchant transporterConfig booking routeCode = do
  mappings <- filter ((== booking.merchantOperatingCityId) . (.merchantOperatingCityId)) <$> QVRM.findAllByRouteCode routeCode
  if null mappings
    then pure (Assign.Unassigned $ "no vehicle is bound to route " <> routeCode <> "; the run is on the scheduled board")
    else do
      (errors, candidates) <- partitionEithers <$> mapM boundCandidate mappings
      outcome <- Assign.assignFirstAvailable merchant transporterConfig booking candidates
      pure $ case outcome of
        Assign.Unassigned why | not (null errors) -> Assign.Unassigned (T.intercalate "; " (errors <> [why | not (T.null why)]))
        _ -> outcome
  where
    boundCandidate mapping = do
      res <- withTryCatch "sharedRun:assignBoundVehicle" $ do
        plate <- decrypt mapping.vehicleNumber
        vehicle <- QVehicle.findByRegistrationNo plate >>= fromMaybeM (InvalidRequest $ "vehicle " <> plate <> " bound to route " <> routeCode <> " is not registered")
        driver <- QPerson.findById vehicle.driverId >>= fromMaybeM (PersonNotFound vehicle.driverId.getId)
        pure Assign.RunCandidate {driver, fleetOwnerId = mapping.fleetOwnerId, label = "the driver of " <> plate}
      pure (either (Left . show) Right res)
