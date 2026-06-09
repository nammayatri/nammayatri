{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.RideFlowDebug
  ( getRideFlowDebug,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Trip as DTrip
import Environment
import EulerHS.Prelude ((<|>))
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Id
import qualified Storage.Clickhouse.Estimate as CHEstimate
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverQuote as QDriverQuote
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST

-- | Detected flow type based on tripCategory
data FlowType = DynamicOfferFlow | StaticOfferFlow | RideOtpFlow | UnknownFlow
  deriving (Show)

detectFlowType :: Maybe DBooking.Booking -> [CHEstimate.Estimate] -> [DQuote.Quote] -> FlowType
detectFlowType mbBooking estimates quotes =
  case (.tripCategory) <$> mbBooking of
    Just tc
      | DTrip.isDynamicOfferTrip tc -> DynamicOfferFlow
      | DTrip.isRideOtpTrip tc -> RideOtpFlow
      | otherwise -> StaticOfferFlow
    Nothing ->
      case (.tripCategory) <$> listToMaybe estimates of
        Just tc | DTrip.isDynamicOfferTrip tc -> DynamicOfferFlow
        _ ->
          case (.tripCategory) <$> listToMaybe quotes of
            Just tc
              | DTrip.isRideOtpTrip tc -> RideOtpFlow
              | otherwise -> StaticOfferFlow
            Nothing
              | not (null estimates) -> DynamicOfferFlow
              | not (null quotes) -> StaticOfferFlow
              | otherwise -> UnknownFlow

getRideFlowDebug ::
  ShortId DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe (ShortId Common.Ride) ->
  Flow Common.RideFlowDebugRes
getRideFlowDebug _merchantShortId _merchantOpCityId mbRideId mbBookingId mbSearchRequestId mbRideShortId = do
  -- Step 1: Resolve the chain from whatever ID we have
  (mbRide, mbBooking, mbSearchReq) <- resolveChain mbRideId mbBookingId mbSearchRequestId mbRideShortId

  -- Step 2: If we still don't have a SearchRequest, try via booking's transactionId
  mbSearchReq' <- case mbSearchReq of
    Just sr -> pure (Just sr)
    Nothing -> case mbBooking of
      Just booking -> QSR.findByTransactionId booking.transactionId
      Nothing -> pure Nothing

  -- Step 3: Find downstream entities
  searchTries <- maybe (pure []) (QST.findAllByRequestId . (.id)) mbSearchReq'
  estimates <- maybe (pure []) (\sr -> CHEstimate.findAllByRequestId sr.id sr.createdAt) mbSearchReq'
  driverQuotes <- fmap concat . forM searchTries $ QDriverQuote.findAllBySTIdIgnoringStatus . (.id)
  bppQuotes <- maybe (pure []) (QQuote.findAllBySearchRequestId . (.id)) mbSearchReq'

  -- Step 4: If we don't have booking yet, try from search tries
  mbBooking' <- case mbBooking of
    Just b -> pure (Just b)
    Nothing -> case searchTries of
      (st : _) -> QBooking.findBySTId st.id
      _ -> case mbSearchReq' of
        Just sr -> QBooking.findByTransactionId sr.transactionId
        Nothing -> pure Nothing

  -- Step 5: If we don't have ride yet, try from booking
  mbRide' <- case mbRide of
    Just r -> pure (Just r)
    Nothing -> maybe (pure Nothing) (QRide.findOneByBookingId . (.id)) mbBooking'

  -- Step 6: Detect flow type and build timeline
  let flowType = detectFlowType mbBooking' estimates bppQuotes
  let timeline = buildTimeline flowType mbSearchReq' searchTries estimates bppQuotes driverQuotes mbBooking' mbRide'
  let currentStage = computeCurrentStage flowType mbSearchReq' searchTries estimates bppQuotes driverQuotes mbBooking' mbRide'
  let tripCat = (show . (.tripCategory) <$> mbBooking') <|> (show . (.tripCategory) <$> listToMaybe estimates) <|> (show . (.tripCategory) <$> listToMaybe bppQuotes)
  let issues = detectIssues flowType mbSearchReq' searchTries estimates bppQuotes driverQuotes mbBooking' mbRide'

  pure
    Common.RideFlowDebugRes
      { currentStage = currentStage,
        tripCategory = tripCat,
        timeline = timeline,
        bapData = Nothing, -- Populated by dashboard when it calls rider-app
        bppData =
          Just
            Common.BPPSideDebug
              { searchRequest = mkSearchRequestDebug <$> mbSearchReq',
                searchTries = map mkSearchTryDebug searchTries,
                estimates = map mkEstimateDebug estimates,
                driverQuotes = map mkDriverQuoteDebug driverQuotes,
                booking = mkBookingDebug <$> mbBooking',
                ride = mkRideDebug <$> mbRide'
              },
        crossReferenceIds =
          Common.CrossReferenceIds
            { transactionId = (.transactionId) <$> mbSearchReq',
              bapSearchRequestId = Nothing,
              bppSearchRequestId = (.getId) . (.id) <$> mbSearchReq',
              bapBookingId = Nothing,
              bppBookingId = (.getId) . (.id) <$> mbBooking',
              bapRideId = Nothing,
              bppRideId = (.getId) . (.id) <$> mbRide',
              rideShortId = (.getShortId) . (.shortId) <$> mbRide'
            },
        issues = issues
      }

-- | Resolve from whatever ID the caller provided
resolveChain ::
  Maybe (Id Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe (ShortId Common.Ride) ->
  Flow (Maybe DRide.Ride, Maybe DBooking.Booking, Maybe DSR.SearchRequest)
resolveChain mbRideId mbBookingId mbSearchRequestId mbRideShortId = do
  mbRide <- case mbRideShortId of
    Just shortId -> QRide.findRideByRideShortId (ShortId $ getShortId shortId)
    Nothing -> case mbRideId of
      Just rideId -> QRide.findById (cast rideId)
      Nothing -> pure Nothing

  mbBooking <- case mbRide of
    Just ride -> QBooking.findById ride.bookingId
    Nothing -> case mbBookingId of
      Just bookingId -> QBooking.findById (Id bookingId)
      Nothing -> pure Nothing

  mbSearchReq <- case mbBooking of
    Just booking -> QSR.findByTransactionId booking.transactionId
    Nothing -> case mbSearchRequestId of
      Just srId -> QSR.findById (Id srId)
      Nothing -> pure Nothing

  pure (mbRide, mbBooking, mbSearchReq)

-- | Compute current flow stage
computeCurrentStage ::
  FlowType ->
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [CHEstimate.Estimate] ->
  [DQuote.Quote] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  Common.FlowStage
computeCurrentStage flowType mbSR searchTries estimates quotes driverQuotes mbBooking mbRide =
  case mbRide of
    Just ride -> case ride.status of
      DRide.NEW -> Common.BAP_ON_CONFIRM_RECEIVED
      DRide.INPROGRESS -> Common.BPP_RIDE_STARTED
      DRide.COMPLETED -> Common.BPP_RIDE_COMPLETED
      DRide.CANCELLED -> Common.FLOW_CANCELLED
      DRide.UPCOMING -> Common.BAP_ON_CONFIRM_RECEIVED
    Nothing -> case mbBooking of
      Just booking -> case booking.status of
        DBooking.CANCELLED -> Common.FLOW_CANCELLED
        DBooking.COMPLETED -> Common.BPP_RIDE_COMPLETED
        DBooking.REALLOCATED -> Common.BPP_DRIVER_SEARCH_STARTED
        DBooking.TRIP_ASSIGNED -> Common.BPP_CONFIRM_PROCESSED
        DBooking.NEW -> case flowType of
          RideOtpFlow -> case booking.specialZoneOtpCode of
            Just _ -> Common.BPP_CONFIRM_PROCESSED -- OTP generated, waiting for driver
            Nothing -> Common.BPP_BOOKING_CREATED
          StaticOfferFlow ->
            if not (null searchTries)
              then Common.BPP_DRIVER_SEARCH_STARTED -- Confirm triggered driver search
              else Common.BPP_BOOKING_CREATED
          _ -> Common.BPP_BOOKING_CREATED
      Nothing -> case flowType of
        DynamicOfferFlow -> case (searchTries, driverQuotes) of
          (_, _ : _) -> Common.BPP_DRIVER_QUOTES_RECEIVED
          (_ : _, []) -> case listToMaybe searchTries of
            Just st
              | st.status == DST.ACTIVE -> Common.BPP_DRIVER_SEARCH_STARTED
              | st.status == DST.COMPLETED -> Common.BPP_DRIVER_QUOTES_RECEIVED
              | st.status == DST.CANCELLED -> Common.FLOW_CANCELLED
            _ -> Common.BPP_DRIVER_SEARCH_STARTED
          _ ->
            if not (null estimates)
              then Common.BPP_ESTIMATES_CREATED
              else
                if isJust mbSR
                  then Common.BPP_SEARCH_RECEIVED
                  else Common.FLOW_UNKNOWN
        _ ->
          -- Static / RideOTP
          if not (null quotes)
            then Common.BAP_ON_SEARCH_RECEIVED
            else
              if isJust mbSR
                then Common.BPP_SEARCH_RECEIVED
                else Common.FLOW_UNKNOWN

-- | Build flow-specific timeline
buildTimeline ::
  FlowType ->
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [CHEstimate.Estimate] ->
  [DQuote.Quote] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Common.FlowStageEntry]
buildTimeline flowType mbSR searchTries estimates quotes driverQuotes mbBooking mbRide =
  case flowType of
    DynamicOfferFlow -> buildDynamicTimeline mbSR searchTries estimates driverQuotes mbBooking mbRide
    StaticOfferFlow -> buildStaticTimeline mbSR searchTries quotes mbBooking mbRide
    RideOtpFlow -> buildRideOtpTimeline mbSR quotes mbBooking mbRide
    UnknownFlow -> buildDynamicTimeline mbSR searchTries estimates driverQuotes mbBooking mbRide -- fallback

-- =============================================
-- DYNAMIC OFFER FLOW TIMELINE
-- Search → Estimates → on_search
-- → Select → SearchTry/DriverSearch → DriverQuotes → on_select
-- → Init → BPP Booking → on_init (BAP Booking + payment)
-- → Confirm → Ride created (driver assigned) → on_confirm (RideAssigned)
-- → Start → End
-- =============================================
buildDynamicTimeline ::
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [CHEstimate.Estimate] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Common.FlowStageEntry]
buildDynamicTimeline mbSR searchTries estimates driverQuotes mbBooking mbRide =
  let hasSR = isJust mbSR
      hasEst = not (null estimates)
      hasST = not (null searchTries)
      hasDQ = not (null driverQuotes)
      hasBooking = isJust mbBooking
      hasRide = isJust mbRide
   in catMaybes
        [ -- Phase 1: Search
          Just $ entry Common.BPP_SEARCH_RECEIVED (done hasSR) ((.createdAt) <$> mbSR) (Just "BPP received search, created SearchRequest"),
          Just $ entry Common.BPP_ESTIMATES_CREATED (done hasEst) (CH.getDateTime . (.createdAt) <$> listToMaybe estimates) (Just $ show (length estimates) <> " estimates (fare ranges)"),
          Just $ entry Common.BAP_ON_SEARCH_RECEIVED (done hasEst) (CH.getDateTime . (.createdAt) <$> listToMaybe estimates) (Just "BAP received estimates via on_search"),
          -- Phase 2: Select → Driver matching
          Just $ entry Common.BAP_SELECT_SENT (done hasST) ((.createdAt) <$> listToMaybe searchTries) (Just "Rider selected estimate, BAP sent select"),
          Just $
            entry
              Common.BPP_DRIVER_SEARCH_STARTED
              (if hasST then (if any (\st -> st.status == DST.ACTIVE) searchTries then Common.STAGE_CURRENT else Common.STAGE_DONE) else Common.STAGE_PENDING)
              ((.createdAt) <$> listToMaybe searchTries)
              (Just $ show (length searchTries) <> " search tries, drivers notified"),
          Just $ entry Common.BPP_DRIVER_QUOTES_RECEIVED (done hasDQ) ((.createdAt) <$> listToMaybe driverQuotes) (Just $ show (length driverQuotes) <> " driver quotes"),
          Just $ entry Common.BAP_ON_SELECT_RECEIVED (done hasDQ) ((.createdAt) <$> listToMaybe driverQuotes) (Just "BAP received driver offers via on_select"),
          -- Phase 3: Init → Booking
          Just $ entry Common.BAP_INIT_SENT (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP sent init (rider accepted a driver offer)"),
          Just $ entry Common.BPP_BOOKING_CREATED (done hasBooking) ((.createdAt) <$> mbBooking) (bookingDetail mbBooking),
          Just $ entry Common.BAP_ON_INIT_RECEIVED (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP received on_init, booking + payment info created"),
          -- Phase 4: Confirm → Ride created (driver already assigned from quote)
          Just $ entry Common.BAP_CONFIRM_SENT (done hasRide) ((.createdAt) <$> mbRide) (Just "BAP sent confirm"),
          Just $ entry Common.BPP_CONFIRM_PROCESSED (done hasRide) ((.createdAt) <$> mbRide) (Just $ if hasRide then "Ride created, driver assigned" else "Pending driver assignment"),
          Just $ entry Common.BAP_ON_CONFIRM_RECEIVED (done hasRide) ((.createdAt) <$> mbRide) (rideDriverDetail mbRide),
          -- Cancellation or ride lifecycle
          rideLifecycleEntries mbBooking mbRide
        ]
        <> rideLifecycleList mbBooking mbRide

-- =============================================
-- STATIC OFFER FLOW TIMELINE
-- Search → Quotes → on_search
-- → Init → BPP Booking → on_init (BAP Booking)
-- → Confirm → on_confirm (BookingConfirmed, no ride yet)
-- → SearchTry/DriverSearch → Driver found → Ride → on_update (RideAssigned)
-- → Start → End
-- =============================================
buildStaticTimeline ::
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [DQuote.Quote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Common.FlowStageEntry]
buildStaticTimeline mbSR searchTries quotes mbBooking mbRide =
  let hasSR = isJust mbSR
      hasQuotes = not (null quotes)
      hasST = not (null searchTries)
      hasBooking = isJust mbBooking
      hasRide = isJust mbRide
   in catMaybes
        [ -- Phase 1: Search
          Just $ entry Common.BPP_SEARCH_RECEIVED (done hasSR) ((.createdAt) <$> mbSR) (Just "BPP received search, created SearchRequest"),
          Just $ entry Common.BAP_ON_SEARCH_RECEIVED (done hasQuotes) ((.createdAt) <$> listToMaybe quotes) (Just $ show (length quotes) <> " quotes (fixed fare) via on_search"),
          -- Phase 2: Init → Booking (NO select step in static flow)
          Just $ entry Common.BAP_INIT_SENT (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP sent init directly (no select in static flow)"),
          Just $ entry Common.BPP_BOOKING_CREATED (done hasBooking) ((.createdAt) <$> mbBooking) (bookingDetail mbBooking),
          Just $ entry Common.BAP_ON_INIT_RECEIVED (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP received on_init, booking created"),
          -- Phase 3: Confirm → on_confirm sent immediately (BookingConfirmed, no ride)
          Just $ entry Common.BAP_CONFIRM_SENT (done hasST) ((.createdAt) <$> listToMaybe searchTries) (Just "BAP sent confirm"),
          Just $ entry Common.BPP_CONFIRM_PROCESSED (done hasST) ((.createdAt) <$> listToMaybe searchTries) (Just $ if hasST then "on_confirm sent (BookingConfirmed), driver search initiated" else "Pending"),
          Just $ entry Common.BAP_ON_CONFIRM_RECEIVED (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP received on_confirm (BookingConfirmed, no driver yet)"),
          -- Phase 4: Async driver search after confirm
          Just $
            entry
              Common.BPP_DRIVER_SEARCH_STARTED
              (if hasST then (if any (\st -> st.status == DST.ACTIVE) searchTries then Common.STAGE_CURRENT else Common.STAGE_DONE) else Common.STAGE_PENDING)
              ((.createdAt) <$> listToMaybe searchTries)
              (Just $ show (length searchTries) <> " search tries"),
          -- Driver found → Ride created → on_update (RideAssigned) sent to BAP
          Just $ entry Common.BAP_RIDE_STARTED (done hasRide) ((.createdAt) <$> mbRide) (maybe (Just "Waiting for driver to accept") (\r -> Just $ "Driver assigned via on_update, driverId=" <> r.driverId.getId) mbRide),
          rideLifecycleEntries mbBooking mbRide
        ]
        <> rideLifecycleList mbBooking mbRide

-- =============================================
-- RIDE OTP FLOW TIMELINE
-- Search → Quotes → on_search
-- → Init → BPP Booking → on_init (BAP Booking)
-- → Confirm → OTP generated → on_confirm (BookingConfirmed + OTP)
-- → Driver scans OTP → Ride created → on_update (RideAssigned)
-- → Start → End
-- =============================================
buildRideOtpTimeline ::
  Maybe DSR.SearchRequest ->
  [DQuote.Quote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Common.FlowStageEntry]
buildRideOtpTimeline mbSR quotes mbBooking mbRide =
  let hasSR = isJust mbSR
      hasQuotes = not (null quotes)
      hasBooking = isJust mbBooking
      hasOtp = isJust (mbBooking >>= (.specialZoneOtpCode))
      hasRide = isJust mbRide
   in catMaybes
        [ -- Phase 1: Search
          Just $ entry Common.BPP_SEARCH_RECEIVED (done hasSR) ((.createdAt) <$> mbSR) (Just "BPP received search, created SearchRequest"),
          Just $ entry Common.BAP_ON_SEARCH_RECEIVED (done hasQuotes) ((.createdAt) <$> listToMaybe quotes) (Just $ show (length quotes) <> " quotes (fixed fare) via on_search"),
          -- Phase 2: Init → Booking (NO select step in RideOTP flow)
          Just $ entry Common.BAP_INIT_SENT (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP sent init directly (no select in RideOTP flow)"),
          Just $ entry Common.BPP_BOOKING_CREATED (done hasBooking) ((.createdAt) <$> mbBooking) (bookingDetail mbBooking),
          Just $ entry Common.BAP_ON_INIT_RECEIVED (done hasBooking) ((.createdAt) <$> mbBooking) (Just "BAP received on_init, booking created"),
          -- Phase 3: Confirm → OTP generated (no driver assigned yet)
          Just $ entry Common.BAP_CONFIRM_SENT (done hasOtp) ((.updatedAt) <$> mbBooking) (Just "BAP sent confirm"),
          Just $
            entry
              Common.BPP_CONFIRM_PROCESSED
              (done hasOtp)
              ((.updatedAt) <$> mbBooking)
              (Just $ maybe "Pending" (\b -> maybe "Pending OTP generation" (\otp -> "OTP generated: " <> otp) b.specialZoneOtpCode) mbBooking),
          Just $ entry Common.BAP_ON_CONFIRM_RECEIVED (done hasOtp) ((.updatedAt) <$> mbBooking) (Just "BAP received on_confirm (BookingConfirmed + OTP, no driver yet)"),
          -- Phase 4: Driver scans OTP → Ride created → on_update (RideAssigned) sent to BAP
          Just $
            entry
              Common.BAP_RIDE_STARTED -- conceptually "ride created via OTP scan"
              (done hasRide)
              ((.createdAt) <$> mbRide)
              (maybe (Just "Waiting for driver to scan OTP") (\r -> Just $ "Driver scanned OTP, ride created via on_update, driverId=" <> r.driverId.getId) mbRide),
          rideLifecycleEntries mbBooking mbRide
        ]
        <> rideLifecycleList mbBooking mbRide

-- | Common ride lifecycle entries (start → end) shared by all flows
rideLifecycleEntries :: Maybe DBooking.Booking -> Maybe DRide.Ride -> Maybe Common.FlowStageEntry
rideLifecycleEntries mbBooking mbRide =
  let bStatus = (.status) <$> mbBooking
      rStatus = (.status) <$> mbRide
      isCancelled = bStatus == Just DBooking.CANCELLED || rStatus == Just DRide.CANCELLED
   in if isCancelled
        then Just $ entry Common.FLOW_CANCELLED Common.STAGE_CURRENT (((.updatedAt) <$> mbRide) <|> ((.updatedAt) <$> mbBooking)) (Just $ maybe "" (\b -> "bookingStatus=" <> show b.status) mbBooking)
        else Nothing

rideLifecycleList :: Maybe DBooking.Booking -> Maybe DRide.Ride -> [Common.FlowStageEntry]
rideLifecycleList mbBooking mbRide =
  let rStatus = (.status) <$> mbRide
      bStatus = (.status) <$> mbBooking
      hasRide = isJust mbRide
      isCancelled = bStatus == Just DBooking.CANCELLED || rStatus == Just DRide.CANCELLED
   in if isCancelled
        then []
        else
          catMaybes
            [ if hasRide
                then
                  Just $
                    entry
                      Common.BPP_RIDE_STARTED
                      (case rStatus of Just DRide.INPROGRESS -> Common.STAGE_CURRENT; Just DRide.COMPLETED -> Common.STAGE_DONE; _ -> Common.STAGE_PENDING)
                      (mbRide >>= (.tripStartTime))
                      (Just "Driver started ride")
                else Nothing,
              if hasRide
                then
                  Just $
                    entry
                      Common.BPP_RIDE_COMPLETED
                      (if rStatus == Just DRide.COMPLETED then Common.STAGE_DONE else Common.STAGE_PENDING)
                      (mbRide >>= (.tripEndTime))
                      ((\r -> "fare=" <> maybe "N/A" show r.fare <> ", endedBy=" <> maybe "N/A" show r.rideEndedBy) <$> mbRide)
                else Nothing
            ]

-- | Helpers
done :: Bool -> Common.FlowStageStatus
done True = Common.STAGE_DONE
done False = Common.STAGE_PENDING

entry :: Common.FlowStage -> Common.FlowStageStatus -> Maybe UTCTime -> Maybe Text -> Common.FlowStageEntry
entry stage status timestamp detail = Common.FlowStageEntry {..}

bookingDetail :: Maybe DBooking.Booking -> Maybe Text
bookingDetail = fmap (\b -> "status=" <> show b.status <> ", tripCategory=" <> show b.tripCategory)

rideDriverDetail :: Maybe DRide.Ride -> Maybe Text
rideDriverDetail = fmap (\r -> "driverId=" <> r.driverId.getId)

-- | Detect potential issues (flow-aware)
detectIssues ::
  FlowType ->
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [CHEstimate.Estimate] ->
  [DQuote.Quote] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Text]
detectIssues flowType mbSR searchTries estimates quotes driverQuotes mbBooking mbRide =
  catMaybes
    [ if isNothing mbSR then Just "BPP SearchRequest not found - search may not have reached BPP" else Nothing,
      -- Flow-specific issues
      case flowType of
        DynamicOfferFlow
          | isJust mbSR && null estimates -> Just "No estimates created - fare policy may not match or no eligible vehicle tiers"
          | not (null estimates) && null searchTries -> Just "Estimates exist but no SearchTry - BAP may not have sent select"
          | not (null searchTries) && all (\st -> st.status == DST.CANCELLED) searchTries -> Just "All SearchTries cancelled - no driver accepted or search timed out"
          | not (null searchTries) && null driverQuotes && any (\st -> st.status == DST.COMPLETED) searchTries -> Just "SearchTry completed but no driver quotes found"
          | otherwise -> Nothing
        StaticOfferFlow
          | isJust mbSR && null quotes -> Just "No quotes created - fare policy may not match"
          | isJust mbBooking && null searchTries -> Just "Booking exists but no SearchTry - confirm may not have triggered driver search"
          | not (null searchTries) && all (\st -> st.status == DST.CANCELLED) searchTries -> Just "All SearchTries cancelled - no driver accepted"
          | otherwise -> Nothing
        RideOtpFlow
          | isJust mbSR && null quotes -> Just "No quotes created - fare policy may not match"
          | isJust mbBooking && isNothing (mbBooking >>= (.specialZoneOtpCode)) -> Just "Booking exists but no OTP generated - confirm may have failed"
          | isJust (mbBooking >>= (.specialZoneOtpCode)) && isNothing mbRide -> Just "OTP generated but no ride - waiting for driver to scan OTP"
          | otherwise -> Nothing
        UnknownFlow -> Just "Could not determine flow type (dynamic/static/rideOTP) - check tripCategory",
      -- Common issues
      if ((.distanceCalculationFailed) <$> mbRide) == Just (Just True) then Just "Distance calculation failed for this ride" else Nothing,
      if ((.status) <$> mbBooking) == Just DBooking.REALLOCATED then Just "Booking was reallocated - original driver cancelled" else Nothing
    ]

-- | Mapper functions: domain types -> API debug types
mkSearchRequestDebug :: DSR.SearchRequest -> Common.BPPSearchRequestDebug
mkSearchRequestDebug sr =
  Common.BPPSearchRequestDebug
    { id = sr.id.getId,
      transactionId = Just sr.transactionId,
      createdAt = sr.createdAt,
      estimatedDistance = sr.estimatedDistance,
      estimatedDuration = sr.estimatedDuration,
      autoAssignEnabled = sr.autoAssignEnabled,
      area = show <$> sr.area
    }

mkSearchTryDebug :: DST.SearchTry -> Common.BPPSearchTryDebug
mkSearchTryDebug st =
  Common.BPPSearchTryDebug
    { id = st.id.getId,
      status = show st.status,
      vehicleServiceTier = show st.vehicleServiceTier,
      searchRepeatCounter = st.searchRepeatCounter,
      searchRepeatType = show st.searchRepeatType,
      tripCategory = show st.tripCategory,
      createdAt = st.createdAt,
      validTill = st.validTill
    }

mkEstimateDebug :: CHEstimate.Estimate -> Common.BPPEstimateDebug
mkEstimateDebug est =
  Common.BPPEstimateDebug
    { id = est.id.getId,
      tripCategory = show est.tripCategory,
      vehicleServiceTier = show est.vehicleServiceTier,
      minFare = est.minFare,
      maxFare = est.maxFare,
      createdAt = CH.getDateTime est.createdAt
    }

mkDriverQuoteDebug :: DDQ.DriverQuote -> Common.BPPDriverQuoteDebug
mkDriverQuoteDebug dq =
  Common.BPPDriverQuoteDebug
    { id = dq.id.getId,
      driverId = dq.driverId.getId,
      driverName = dq.driverName,
      status = show dq.status,
      estimatedFare = dq.estimatedFare,
      validTill = dq.validTill,
      distanceToPickup = dq.distanceToPickup,
      durationToPickup = dq.durationToPickup,
      createdAt = dq.createdAt
    }

mkBookingDebug :: DBooking.Booking -> Common.BPPBookingDebug
mkBookingDebug b =
  Common.BPPBookingDebug
    { id = b.id.getId,
      status = show b.status,
      tripCategory = show b.tripCategory,
      estimatedFare = b.estimatedFare,
      riderId = (.getId) <$> b.riderId,
      specialZoneOtpCode = b.specialZoneOtpCode,
      searchTryId = (.getId) <$> b.searchTryId,
      createdAt = b.createdAt,
      updatedAt = b.updatedAt
    }

mkRideDebug :: DRide.Ride -> Common.BPPRideDebug
mkRideDebug r =
  Common.BPPRideDebug
    { id = r.id.getId,
      shortId = r.shortId.getShortId,
      status = show r.status,
      driverId = r.driverId.getId,
      otp = r.otp,
      endOtp = r.endOtp,
      fare = r.fare,
      tripStartTime = r.tripStartTime,
      tripEndTime = r.tripEndTime,
      chargeableDistance = r.chargeableDistance,
      distanceCalculationFailed = r.distanceCalculationFailed,
      rideEndedBy = show <$> r.rideEndedBy,
      createdAt = r.createdAt,
      updatedAt = r.updatedAt
    }
