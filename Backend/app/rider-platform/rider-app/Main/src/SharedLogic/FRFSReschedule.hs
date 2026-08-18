module SharedLogic.FRFSReschedule where

import qualified Data.Time as Time
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingPaymentCategory as DTBPC
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified SharedLogic.FRFSSeatBooking as SeatBooking
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.FRFSConfig as CQFRFS
import Storage.CachedQueries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Person as CQP
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.FRFSTicketBookingPaymentCategory as QFRFSTicketBookingPaymentCategory
import Tools.Error

validateRescheduleEligibility ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  Text ->
  Text -> -- new boarding stop code (defaults upstream to the old one)
  Text -> -- new destination stop code (defaults upstream to the old one)
  Text -> -- new route code (defaults upstream to the old one)
  DIBC.IntegratedBPPConfig ->
  m ()
validateRescheduleEligibility oldBooking newTripId newFromCode newToCode newRouteCode integratedBppConfig = do
  unless (oldBooking.status == DFRFSTicketBookingStatus.CONFIRMED) $
    throwError $ InvalidRequest "Booking is not confirmed, cannot be rescheduled"
  frfsConfig <-
    CQFRFS.findByMerchantOperatingCityId oldBooking.merchantOperatingCityId (Just [])
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> oldBooking.merchantOperatingCityId.getId)
  unless (fromMaybe False frfsConfig.isRescheduleAllowed) $
    throwError $ InvalidRequest "Reschedule is not enabled for this city"
  serviceTierType <-
    FRFSUtils.getServiceTierTypeFromRouteStationsJson oldBooking.routeStationsJson
      & fromMaybeM (InvalidRequest "Cannot determine service tier for this booking, reschedule not supported")
  vst <-
    QFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTierType oldBooking.merchantOperatingCityId integratedBppConfig.id
      >>= fromMaybeM (InvalidRequest "Reschedule is not enabled for this route/vehicle/service tier")
  unless (fromMaybe False vst.isRescheduleAllowed) $
    throwError $ InvalidRequest "Reschedule is not enabled for this service tier"
  when (fromMaybe 0 oldBooking.rescheduleCount >= fromMaybe 1 vst.maxRescheduleCount) $
    throwError $ InvalidRequest "Maximum number of reschedules exceeded for this booking"
  pastWindow <- isPastRescheduleWindow oldBooking (fromMaybe (Seconds 1800) vst.maxRescheduleTimeAfterStart)
  when pastWindow $ throwError $ InvalidRequest "Reschedule window has passed for this booking"
  -- When the rider moves to a different boarding/alighting stop, it must stay within the same cluster as the
  -- original (nearby-equivalent stop). Same-stop reschedules (trip-only) skip this. Fare is enforced separately.
  let stopsChanged = newFromCode /= oldBooking.fromStationCode || newToCode /= oldBooking.toStationCode
  when stopsChanged $ do
    oldFromStation <- OTPRest.getStationByGtfsIdAndStopCode oldBooking.fromStationCode integratedBppConfig >>= fromMaybeM (InvalidRequest $ "Invalid original from station: " <> oldBooking.fromStationCode)
    newFromStation <- OTPRest.getStationByGtfsIdAndStopCode newFromCode integratedBppConfig >>= fromMaybeM (InvalidRequest $ "Invalid from station: " <> newFromCode)
    oldToStation <- OTPRest.getStationByGtfsIdAndStopCode oldBooking.toStationCode integratedBppConfig >>= fromMaybeM (InvalidRequest $ "Invalid original to station: " <> oldBooking.toStationCode)
    newToStation <- OTPRest.getStationByGtfsIdAndStopCode newToCode integratedBppConfig >>= fromMaybeM (InvalidRequest $ "Invalid to station: " <> newToCode)
    unless (isJust newFromStation.clusterId && newFromStation.clusterId == oldFromStation.clusterId) $
      throwError $ InvalidRequest "New boarding stop must be in the same cluster as the original boarding stop"
    unless (isJust newToStation.clusterId && newToStation.clusterId == oldToStation.clusterId) $
      throwError $ InvalidRequest "New destination stop must be in the same cluster as the original destination stop"
  newTripStart <- getNewTripStartTime newTripId newRouteCode newFromCode newToCode integratedBppConfig
  now <- getCurrentTime
  riderConfig <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = oldBooking.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (RiderConfigNotFound oldBooking.merchantOperatingCityId.getId)
  let tzDiff = secondsToNominalDiffTime riderConfig.timeDiffFromUtc
      todayLocal = Time.utctDay (Time.addUTCTime tzDiff now)
      lastAllowedDayLocal = Time.addDays (fromIntegral (fromMaybe 0 vst.maxRescheduleDaysAhead)) todayLocal
      windowEndUtc = Time.addUTCTime (negate tzDiff) (Time.UTCTime (Time.addDays 1 lastAllowedDayLocal) 0)
  when (newTripStart < now || newTripStart >= windowEndUtc) $
    throwError $ InvalidRequest "Selected trip is outside the allowed reschedule window"

getNewTripStartTime ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  Text ->
  Text ->
  Text ->
  Text ->
  DIBC.IntegratedBPPConfig ->
  m UTCTime
getNewTripStartTime tripId routeCode boardingStopCode alightingStopCode integratedBppConfig = do
  let (waybillNo, tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId tripId
  eSchedule <- withTryCatch "FRFSReschedule:getNewTripStartTime" (OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBppConfig)
  schedule <- case eSchedule of
    Left err -> do
      logError $ "FRFSReschedule:getNewTripStartTime failed to fetch bus trip schedule for tripId=" <> tripId <> ": " <> show err
      throwError $ InvalidRequest "Could not verify the selected trip schedule, please try again"
    Right s -> pure s
  let allEtas = concatMap (.eta) schedule
  boardingEta <-
    find (\e -> e.stopCode == boardingStopCode) allEtas
      & fromMaybeM (InvalidRequest "Selected trip does not stop at the boarding station")
  alightingEta <-
    find (\e -> e.stopCode == alightingStopCode) allEtas
      & fromMaybeM (InvalidRequest "Selected trip does not stop at the destination station")
  when (alightingEta.arrivalTimeUnix <= boardingEta.arrivalTimeUnix) $
    throwError $ InvalidRequest "Selected trip does not serve the boarding and destination stations in travel order"
  pure $ FRFSUtils.unixToUTC boardingEta.arrivalTimeUnix

isPastRescheduleWindow ::
  MonadFlow m =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  Seconds ->
  m Bool
isPastRescheduleWindow booking maxRescheduleTimeAfterStart =
  case booking.startTime of
    Just startTime -> do
      now <- getCurrentTime
      pure $ now > addUTCTime (fromIntegral (getSeconds maxRescheduleTimeAfterStart)) startTime
    Nothing -> pure False

rescheduleLockKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
rescheduleLockKey bookingId = "FRFS:RESCHEDULE:LOCK:" <> bookingId.getId

withRescheduleLock :: (Redis.HedisFlow m r, MonadIO m, MonadMask m) => Id DFRFSTicketBooking.FRFSTicketBooking -> m a -> m a
withRescheduleLock bookingId action =
  Redis.withLockRedisAndReturnValue (rescheduleLockKey bookingId) 60 action

-- | Mint a fresh internal search + fresh quote/quote-categories for the staging booking (copied from the
-- old ones with the new trip's searchId/validTill/vehicleNumber). The OLD quote/categories are left
-- untouched so the still-live old booking stays correct. Returns the fresh searchId, quote, and categories.
mkFreshSearchAndFreshQuote ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  DFRFSQuote.FRFSQuote ->
  Text ->
  DIBC.IntegratedBPPConfig ->
  Maybe Int ->
  m (Id DFRFSSearch.FRFSSearch, DFRFSQuote.FRFSQuote, [DFRFSQuoteCategory.FRFSQuoteCategory])
mkFreshSearchAndFreshQuote oldBooking oldQuote newTripId integratedBppConfig mbSearchTtlSec = do
  now <- getCurrentTime
  freshSearchId <- generateGUID
  freshQuoteId <- generateGUID
  oldQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId oldQuote.id
  let totalQty = sum (map (.selectedQuantity) oldQuoteCategories)
      validTill' = addUTCTime (maybe 30 intToNominalDiffTime mbSearchTtlSec) now
  let (waybillNo, _tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId newTripId
  eMeta <- withTryCatch "FRFSReschedule:getWaybillMetadata" (OTPRest.getWaybillMetadata waybillNo integratedBppConfig)
  newVehicleNo <- case eMeta of
    Left err -> do
      logError $ "FRFSReschedule:mkFreshSearchAndFreshQuote failed to fetch waybill metadata for tripId=" <> newTripId <> ": " <> show err
      throwError $ InvalidRequest "Could not determine the vehicle for the selected trip, please try again"
    Right meta -> pure meta.vehicle_no
  let freshSearch =
        DFRFSSearch.FRFSSearch
          { busLocationData = [],
            clientBundleVersion = Nothing,
            clientSdkVersion = Nothing,
            cloudType = oldBooking.cloudType,
            fromStationAddress = oldBooking.fromStationAddress,
            fromStationCode = oldBooking.fromStationCode,
            fromStationName = oldBooking.fromStationName,
            fromStationPoint = oldBooking.fromStationPoint,
            hasApplicablePass = Nothing,
            id = freshSearchId,
            integratedBppConfigId = oldBooking.integratedBppConfigId,
            isOnSearchReceived = Nothing,
            isSingleMode = oldBooking.isSingleMode,
            merchantId = oldBooking.merchantId,
            merchantOperatingCityId = oldBooking.merchantOperatingCityId,
            multimodalSearchRequestId = Nothing,
            onSearchFailed = Nothing,
            partnerOrgId = oldBooking.partnerOrgId,
            partnerOrgTransactionId = oldBooking.partnerOrgTransactionId,
            quantity = totalQty,
            recentLocationId = oldBooking.recentLocationId,
            riderId = oldBooking.riderId,
            routeCode = oldBooking.routeCode,
            searchAsParentStops = Nothing,
            toStationAddress = oldBooking.toStationAddress,
            toStationCode = oldBooking.toStationCode,
            toStationName = oldBooking.toStationName,
            toStationPoint = oldBooking.toStationPoint,
            validTill = Just validTill',
            vehicleNumber = Just newVehicleNo,
            vehicleType = oldBooking.vehicleType,
            createdAt = now,
            updatedAt = now
          }
  QFRFSSearch.create freshSearch
  let freshQuote =
        oldQuote
          { DFRFSQuote.id = freshQuoteId,
            DFRFSQuote.searchId = freshSearchId,
            DFRFSQuote.validTill = validTill',
            DFRFSQuote.vehicleNumber = Just newVehicleNo,
            DFRFSQuote.createdAt = now,
            DFRFSQuote.updatedAt = now
          }
  QFRFSQuote.create freshQuote
  freshCategories <-
    mapM
      ( \oldQc -> do
          newQcId <- generateGUID
          pure oldQc {DFRFSQuoteCategory.id = newQcId, DFRFSQuoteCategory.quoteId = freshQuoteId, DFRFSQuoteCategory.createdAt = now, DFRFSQuoteCategory.updatedAt = now}
      )
      oldQuoteCategories
  QFRFSQuoteCategory.createMany freshCategories
  logInfo $ "FRFSReschedule:mkFreshSearchAndFreshQuote oldBookingId=" <> oldBooking.id.getId <> " freshSearchId=" <> freshSearchId.getId <> " freshQuoteId=" <> freshQuoteId.getId
  pure (freshSearchId, freshQuote, freshCategories)

-- | Mint (and persist) a fresh internal search for the staging booking when the rider is moving to a
-- DIFFERENT boarding/alighting stop. Unlike 'mkFreshSearchAndFreshQuote' this does NOT copy the old quote:
-- the caller runs the real Direct search over this fresh search (which produces a genuine quote for the new
-- stops via on_search). Fields are derived from the old booking except the stations/route/point/name/address
-- (resolved for the new stops) and the vehicle number (from the selected trip's waybill). Returns the search.
mkFreshSearchForNewStops ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  DFRFSQuote.FRFSQuote ->
  Text -> -- new trip id (for vehicle + validTill)
  Text -> -- new boarding stop code
  Text -> -- new destination stop code
  Text -> -- new route code
  DIBC.IntegratedBPPConfig ->
  Maybe Int ->
  m DFRFSSearch.FRFSSearch
mkFreshSearchForNewStops oldBooking oldQuote newTripId newFromCode newToCode newRouteCode integratedBppConfig mbSearchTtlSec = do
  now <- getCurrentTime
  freshSearchId <- generateGUID
  oldQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId oldQuote.id
  let totalQty = sum (map (.selectedQuantity) oldQuoteCategories)
      validTill' = addUTCTime (maybe 30 intToNominalDiffTime mbSearchTtlSec) now
  newFromStation <- OTPRest.getStationByGtfsIdAndStopCode newFromCode integratedBppConfig >>= fromMaybeM (InvalidRequest $ "Invalid from station: " <> newFromCode)
  newToStation <- OTPRest.getStationByGtfsIdAndStopCode newToCode integratedBppConfig >>= fromMaybeM (InvalidRequest $ "Invalid to station: " <> newToCode)
  let (waybillNo, _tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId newTripId
  eMeta <- withTryCatch "FRFSReschedule:getWaybillMetadata" (OTPRest.getWaybillMetadata waybillNo integratedBppConfig)
  newVehicleNo <- case eMeta of
    Left err -> do
      logError $ "FRFSReschedule:mkFreshSearchForNewStops failed to fetch waybill metadata for tripId=" <> newTripId <> ": " <> show err
      throwError $ InvalidRequest "Could not determine the vehicle for the selected trip, please try again"
    Right meta -> pure meta.vehicle_no
  let freshSearch =
        DFRFSSearch.FRFSSearch
          { busLocationData = [],
            clientBundleVersion = Nothing,
            clientSdkVersion = Nothing,
            cloudType = oldBooking.cloudType,
            fromStationAddress = newFromStation.address,
            fromStationCode = newFromStation.code,
            fromStationName = Just newFromStation.name,
            fromStationPoint = LatLong <$> newFromStation.lat <*> newFromStation.lon,
            hasApplicablePass = Nothing,
            id = freshSearchId,
            integratedBppConfigId = oldBooking.integratedBppConfigId,
            isOnSearchReceived = Nothing,
            isSingleMode = oldBooking.isSingleMode,
            merchantId = oldBooking.merchantId,
            merchantOperatingCityId = oldBooking.merchantOperatingCityId,
            multimodalSearchRequestId = Nothing,
            onSearchFailed = Nothing,
            partnerOrgId = oldBooking.partnerOrgId,
            partnerOrgTransactionId = oldBooking.partnerOrgTransactionId,
            quantity = totalQty,
            recentLocationId = oldBooking.recentLocationId,
            riderId = oldBooking.riderId,
            routeCode = Just newRouteCode,
            searchAsParentStops = Nothing,
            toStationAddress = newToStation.address,
            toStationCode = newToStation.code,
            toStationName = Just newToStation.name,
            toStationPoint = LatLong <$> newToStation.lat <*> newToStation.lon,
            validTill = Just validTill',
            vehicleNumber = Just newVehicleNo,
            vehicleType = oldBooking.vehicleType,
            createdAt = now,
            updatedAt = now
          }
  QFRFSSearch.create freshSearch
  logInfo $ "FRFSReschedule:mkFreshSearchForNewStops oldBookingId=" <> oldBooking.id.getId <> " freshSearchId=" <> freshSearchId.getId <> " newFrom=" <> newFromCode <> " newTo=" <> newToCode <> " newRoute=" <> newRouteCode
  pure freshSearch

-- | Load-bearing: copy the new seats onto the reused payment's payment-category rows BEFORE the staging
-- confirm reads them (confirm resolves seats from findAllByPaymentId first). Matched 1:1 by category.
syncPaymentCategories ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DFRFSTicketBookingPayment.FRFSTicketBookingPayment ->
  [DFRFSQuoteCategory.FRFSQuoteCategory] ->
  m ()
syncPaymentCategories paymentId updatedQuoteCategories = do
  now <- getCurrentTime
  paymentCategories <- QFRFSTicketBookingPaymentCategory.findAllByPaymentId paymentId
  forM_ paymentCategories $ \pc ->
    case find (\qc -> qc.category == pc.category) updatedQuoteCategories of
      Just qc ->
        QFRFSTicketBookingPaymentCategory.updateByPrimaryKey
          pc
            { DTBPC.seatIds = qc.seatIds,
              DTBPC.holdId = qc.holdId,
              DTBPC.seatLabels = qc.seatLabels,
              DTBPC.updatedAt = now
            }
      Nothing -> pure ()

completeReschedule ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  m ()
completeReschedule oldBookingId stagingBookingId = do
  oldBooking <- QFRFSTicketBooking.findById oldBookingId >>= fromMaybeM (InvalidRequest "Old booking not found while completing reschedule")
  unless (oldBooking.status == DFRFSTicketBookingStatus.RESCHEDULED) $ do
    stagingBooking <- QFRFSTicketBooking.findById stagingBookingId >>= fromMaybeM (InvalidRequest "Staging booking not found while completing reschedule")
    now <- getCurrentTime
    -- Repoint the reused payment to the staging booking. A fully pass-covered reschedule has no payment
    -- row (the pass funded it), so there is nothing to repoint.
    mbPayment <- QFRFSTicketBookingPayment.findTicketBookingPayment stagingBooking
    whenJust mbPayment $ \payment ->
      QFRFSTicketBookingPayment.updateByPrimaryKey
        payment
          { DFRFSTicketBookingPayment.frfsTicketBookingId = stagingBookingId,
            DFRFSTicketBookingPayment.updatedAt = now
          }
    let zeroPrice = mkPrice (Just oldBooking.totalPrice.currency) 0
    void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicketStatus.RESCHEDULED) oldBookingId
    void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById zeroPrice zeroPrice oldBookingId
    void $ CQP.clearPSCache oldBooking.riderId
    oldQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId oldBooking.quoteId
    let oldSeatIds = concat (mapMaybe (.seatIds) oldQuoteCategories)
    case (oldBooking.tripId, oldBooking.fromStopIdx, oldBooking.toStopIdx) of
      (Just tripId, Just fromIdx, Just toIdx)
        | not (null oldSeatIds) -> SeatBooking.releaseConfirmedSeats tripId oldSeatIds fromIdx toIdx
      _ -> pure ()
    -- Commit marker (MUST stay last): finalize the old booking + its tickets only after the payment/recon
    -- migration and seat release above have all succeeded. The guard at the top keys on this RESCHEDULED
    -- status, so a retry after any partial failure re-runs every idempotent step above and lands here once.
    void $ QTicket.updateAllStatusByBookingId DFRFSTicketStatus.RESCHEDULED oldBookingId
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBookingStatus.RESCHEDULED oldBookingId
    logInfo $ "FRFSReschedule:completeReschedule committed oldBookingId=" <> oldBookingId.getId <> " stagingBookingId=" <> stagingBookingId.getId

rollbackFailedReschedule ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  m ()
rollbackFailedReschedule stagingBookingId = do
  stagingBooking <- QFRFSTicketBooking.findById stagingBookingId >>= fromMaybeM (InvalidRequest "Staging booking not found while rolling back reschedule")
  whenJust ((,) <$> stagingBooking.tripId <*> stagingBooking.holdId) $ \(tripId, holdId) ->
    SeatBooking.releaseHold tripId holdId
  whenJust stagingBooking.parentBookingId $ \oldBookingId -> do
    mbOldBooking <- QFRFSTicketBooking.findById oldBookingId
    whenJust mbOldBooking $ \oldBooking -> do
      mbOldPayment <- QFRFSTicketBookingPayment.findTicketBookingPayment oldBooking
      whenJust mbOldPayment $ \oldPayment -> do
        oldQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId oldBooking.quoteId
        syncPaymentCategories oldPayment.id oldQuoteCategories
  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBookingStatus.FAILED stagingBookingId
  logInfo $ "FRFSReschedule:rollbackFailedReschedule stagingBookingId=" <> stagingBookingId.getId
