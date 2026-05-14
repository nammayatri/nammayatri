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
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Environment
import EulerHS.Prelude ((<|>))
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverQuote as QDriverQuote
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST

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
  estimates <- maybe (pure []) (QEstimate.findAllByRequestId . (.id)) mbSearchReq'
  driverQuotes <- fmap concat . forM searchTries $ QDriverQuote.findAllBySTId . (.id)

  -- Step 4: If we don't have booking yet, try from search tries
  mbBooking' <- case mbBooking of
    Just b -> pure (Just b)
    Nothing -> case searchTries of
      (st : _) -> QBooking.findBySTId st.id
      _ -> pure Nothing

  -- Step 5: If we don't have ride yet, try from booking
  mbRide' <- case mbRide of
    Just r -> pure (Just r)
    Nothing -> maybe (pure Nothing) (QRide.findOneByBookingId . (.id)) mbBooking'

  -- Step 6: Build timeline, compute stage, detect issues
  let timeline = buildTimeline mbSearchReq' searchTries estimates driverQuotes mbBooking' mbRide'
  let currentStage = computeCurrentStage mbSearchReq' searchTries driverQuotes mbBooking' mbRide'
  let tripCat = (show . (.tripCategory) <$> mbBooking') <|> (show . (.tripCategory) <$> listToMaybe estimates)
  let issues = detectIssues mbSearchReq' searchTries estimates driverQuotes mbBooking' mbRide'

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
              bapSearchRequestId = Nothing, -- Populated by dashboard from BAP side
              bppSearchRequestId = (.getId) . (.id) <$> mbSearchReq',
              bapBookingId = Nothing, -- Populated by dashboard from BAP side
              bppBookingId = (.getId) . (.id) <$> mbBooking',
              bapRideId = Nothing, -- Populated by dashboard from BAP side
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

-- | Compute current flow stage (now uses the granular BAP/BPP stage names)
computeCurrentStage ::
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  Common.FlowStage
computeCurrentStage mbSR searchTries driverQuotes mbBooking mbRide =
  case mbRide of
    Just ride -> case ride.status of
      DRide.NEW -> Common.BAP_ON_CONFIRM_RECEIVED
      DRide.INPROGRESS -> Common.BPP_RIDE_STARTED
      DRide.COMPLETED -> Common.BPP_RIDE_COMPLETED
      DRide.CANCELLED -> Common.FLOW_CANCELLED
      DRide.UPCOMING -> Common.BAP_ON_CONFIRM_RECEIVED
    Nothing -> case mbBooking of
      Just booking -> case booking.status of
        DBooking.NEW -> Common.BPP_BOOKING_CREATED
        DBooking.TRIP_ASSIGNED -> Common.BPP_CONFIRM_PROCESSED
        DBooking.COMPLETED -> Common.BPP_RIDE_COMPLETED
        DBooking.CANCELLED -> Common.FLOW_CANCELLED
        DBooking.REALLOCATED -> Common.BPP_DRIVER_SEARCH_STARTED
      Nothing -> case (searchTries, driverQuotes) of
        (_, _ : _) -> Common.BPP_DRIVER_QUOTES_RECEIVED
        (_ : _, []) -> case listToMaybe searchTries of
          Just st
            | st.status == DST.ACTIVE -> Common.BPP_DRIVER_SEARCH_STARTED
            | st.status == DST.COMPLETED -> Common.BPP_DRIVER_QUOTES_RECEIVED
            | st.status == DST.CANCELLED -> Common.FLOW_CANCELLED
          _ -> Common.BPP_DRIVER_SEARCH_STARTED
        _ -> case mbSR of
          Just _ -> Common.BPP_SEARCH_RECEIVED
          Nothing -> Common.FLOW_UNKNOWN

-- | Build timeline with all BECKN protocol stages
buildTimeline ::
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [DEst.Estimate] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Common.FlowStageEntry]
buildTimeline mbSR searchTries estimates driverQuotes mbBooking mbRide =
  let hasSR = isJust mbSR
      hasEst = not (null estimates)
      hasST = not (null searchTries)
      hasDQ = not (null driverQuotes)
      hasBooking = isJust mbBooking
      hasRide = isJust mbRide
      bStatus = (.status) <$> mbBooking
      rStatus = (.status) <$> mbRide
      isCancelled = bStatus == Just DBooking.CANCELLED || rStatus == Just DRide.CANCELLED
   in catMaybes
        [ -- Phase 1: Search
          Just $ stageEntry Common.BPP_SEARCH_RECEIVED (if hasSR then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> mbSR) Nothing,
          Just $ stageEntry Common.BPP_ESTIMATES_CREATED (if hasEst then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> listToMaybe estimates) (Just $ show (length estimates) <> " estimates"),
          Just $ stageEntry Common.BAP_ON_SEARCH_RECEIVED (if hasEst then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> listToMaybe estimates) (Just "BAP data available when dashboard merges both sides"),
          -- Phase 2: Select / Driver matching
          Just $
            stageEntry
              Common.BPP_DRIVER_SEARCH_STARTED
              (if hasST then (if any (\st -> st.status == DST.ACTIVE) searchTries then Common.STAGE_CURRENT else Common.STAGE_DONE) else Common.STAGE_PENDING)
              ((.createdAt) <$> listToMaybe searchTries)
              (Just $ show (length searchTries) <> " search tries"),
          Just $ stageEntry Common.BPP_DRIVER_QUOTES_RECEIVED (if hasDQ then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> listToMaybe driverQuotes) (Just $ show (length driverQuotes) <> " driver quotes"),
          -- Phase 3: Init / Booking
          Just $ stageEntry Common.BPP_BOOKING_CREATED (if hasBooking then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> mbBooking) ((\b -> "status=" <> show b.status) <$> mbBooking),
          Just $ stageEntry Common.BAP_ON_INIT_RECEIVED (if hasBooking then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> mbBooking) (Just "BAP booking + payment info created"),
          -- Phase 4: Confirm
          Just $
            stageEntry
              Common.BPP_CONFIRM_PROCESSED
              (if hasRide || bStatus == Just DBooking.TRIP_ASSIGNED || bStatus == Just DBooking.TRIP_ASSIGNED then Common.STAGE_DONE else if hasBooking then Common.STAGE_PENDING else Common.STAGE_PENDING)
              ((.createdAt) <$> mbRide)
              ((\b -> case b.specialZoneOtpCode of Just otp -> "OTP=" <> otp; Nothing -> if isJust mbRide then "ride created" else "pending driver") <$> mbBooking),
          -- Cancellation
          if isCancelled
            then Just $ stageEntry Common.FLOW_CANCELLED Common.STAGE_CURRENT ((.updatedAt) <$> mbBooking) ((\b -> "bookingStatus=" <> show b.status) <$> mbBooking)
            else Nothing,
          -- Phase 5: Ride lifecycle
          if not isCancelled
            then Just $ stageEntry Common.BAP_ON_CONFIRM_RECEIVED (if hasRide || bStatus `elem` [Just DBooking.TRIP_ASSIGNED, Just DBooking.TRIP_ASSIGNED] then Common.STAGE_DONE else Common.STAGE_PENDING) ((.createdAt) <$> mbRide) ((\r -> "driverId=" <> r.driverId.getId) <$> mbRide)
            else Nothing,
          if not isCancelled && hasRide
            then
              Just $
                stageEntry
                  Common.BPP_RIDE_STARTED
                  (case rStatus of Just DRide.INPROGRESS -> Common.STAGE_CURRENT; Just DRide.COMPLETED -> Common.STAGE_DONE; _ -> Common.STAGE_PENDING)
                  (mbRide >>= (.tripStartTime))
                  Nothing
            else Nothing,
          if not isCancelled && hasRide
            then
              Just $
                stageEntry
                  Common.BAP_RIDE_STARTED
                  (case rStatus of Just DRide.INPROGRESS -> Common.STAGE_CURRENT; Just DRide.COMPLETED -> Common.STAGE_DONE; _ -> Common.STAGE_PENDING)
                  (mbRide >>= (.tripStartTime))
                  (Just "on_update sent to BAP")
            else Nothing,
          if not isCancelled && hasRide
            then Just $ stageEntry Common.BPP_RIDE_COMPLETED (if rStatus == Just DRide.COMPLETED then Common.STAGE_DONE else Common.STAGE_PENDING) (mbRide >>= (.tripEndTime)) ((\r -> "fare=" <> maybe "N/A" show r.fare) <$> mbRide)
            else Nothing,
          if not isCancelled && hasRide
            then Just $ stageEntry Common.BAP_RIDE_COMPLETED (if rStatus == Just DRide.COMPLETED then Common.STAGE_DONE else Common.STAGE_PENDING) (mbRide >>= (.tripEndTime)) ((\r -> "rideEndedBy=" <> maybe "N/A" show r.rideEndedBy) <$> mbRide)
            else Nothing
        ]

stageEntry :: Common.FlowStage -> Common.FlowStageStatus -> Maybe UTCTime -> Maybe Text -> Common.FlowStageEntry
stageEntry stage status timestamp detail = Common.FlowStageEntry {..}

-- | Detect potential issues
detectIssues ::
  Maybe DSR.SearchRequest ->
  [DST.SearchTry] ->
  [DEst.Estimate] ->
  [DDQ.DriverQuote] ->
  Maybe DBooking.Booking ->
  Maybe DRide.Ride ->
  [Text]
detectIssues mbSR searchTries estimates driverQuotes mbBooking mbRide =
  catMaybes
    [ if isNothing mbSR then Just "BPP SearchRequest not found - search may not have reached BPP" else Nothing,
      if isJust mbSR && null estimates then Just "No BPP estimates created - fare policy may not match or no eligible vehicle tiers" else Nothing,
      if not (null estimates) && null searchTries then Just "BPP estimates exist but no SearchTry - select may not have been received from BAP" else Nothing,
      if not (null searchTries) && all (\st -> st.status == DST.CANCELLED) searchTries then Just "All SearchTries cancelled - no driver accepted or search timed out" else Nothing,
      if not (null searchTries) && null driverQuotes && any (\st -> st.status == DST.COMPLETED) searchTries then Just "SearchTry completed but no driver quotes - drivers did not respond" else Nothing,
      if isJust mbBooking && isNothing mbRide && ((.status) <$> mbBooking) `elem` [Just DBooking.NEW, Just DBooking.TRIP_ASSIGNED]
        then Just "BPP Booking exists but no ride created - driver assignment pending (check if static/rideOTP flow)"
        else Nothing,
      if ((.distanceCalculationFailed) <$> mbRide) == Just (Just True) then Just "Distance calculation failed for this ride" else Nothing,
      if isJust mbBooking && ((.status) <$> mbBooking) == Just DBooking.REALLOCATED then Just "Booking was reallocated - original driver cancelled" else Nothing
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

mkEstimateDebug :: DEst.Estimate -> Common.BPPEstimateDebug
mkEstimateDebug est =
  Common.BPPEstimateDebug
    { id = est.id.getId,
      tripCategory = show est.tripCategory,
      vehicleServiceTier = show est.vehicleServiceTier,
      minFare = est.minFare,
      maxFare = est.maxFare,
      createdAt = est.createdAt
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
