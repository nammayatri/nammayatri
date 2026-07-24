module SharedLogic.FRFSReschedule where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Ord (comparing)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicketStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified SharedLogic.FRFSCancel as FRFSCancel
import qualified SharedLogic.FRFSFareCalculator as FareCalc
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.FRFSConfig as CQFRFS
import Storage.CachedQueries.FRFSVehicleServiceTier as QFRFSVehicleServiceTier
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Tools.Error

validateRescheduleEligibility ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  DFRFSQuote.FRFSQuote ->
  Text ->
  DIBC.IntegratedBPPConfig ->
  m ()
validateRescheduleEligibility oldBooking newQuote newTripId integratedBppConfig = do
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

  when (fromMaybe 0 oldBooking.rescheduleCount >= fromMaybe 3 vst.maxRescheduleCount) $
    throwError $ InvalidRequest "Maximum number of reschedules exceeded for this booking"

  pastWindow <- isPastRescheduleWindow oldBooking (fromMaybe (Seconds 1800) vst.maxRescheduleTimeAfterStart)
  when pastWindow $ throwError $ InvalidRequest "Reschedule window has passed for this booking"

  unless (newQuote.fromStationCode == oldBooking.fromStationCode && newQuote.toStationCode == oldBooking.toStationCode) $
    throwError $ InvalidRequest "Reschedule is only allowed on the same route and stops"

  newServiceTierType <-
    FRFSUtils.getServiceTierTypeFromRouteStationsJson newQuote.routeStationsJson
      & fromMaybeM (InvalidRequest "Cannot determine service tier for the new quote")
  unless (newServiceTierType == serviceTierType) $
    throwError $ InvalidRequest "Reschedule is only allowed on the same service tier"

  whenJust oldBooking.routeCode $ \routeCode -> do
    mbNewTripStart <- getNewTripStartTime newTripId routeCode newQuote.fromStationCode integratedBppConfig
    whenJust mbNewTripStart $ \newTripStart -> do
      now <- getCurrentTime
      when (newTripStart < now || diffUTCTime newTripStart now > fromIntegral (fromMaybe 7 vst.maxRescheduleDaysAhead) * 86400) $
        throwError $ InvalidRequest "Selected trip is outside the allowed reschedule window"

  newQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId newQuote.id
  let newFareParameters = FareCalc.mkFareParameters (FareCalc.mkCategoryPriceItemFromQuoteCategories newQuoteCategories)
  unless (newFareParameters.totalPrice == oldBooking.totalPrice) $
    throwError $ InvalidRequest "Reschedule is only allowed at the same fare"

getNewTripStartTime ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c) =>
  Text ->
  Text ->
  Text ->
  DIBC.IntegratedBPPConfig ->
  m (Maybe UTCTime)
getNewTripStartTime tripId routeCode boardingStopCode integratedBppConfig = do
  let (waybillNo, tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId tripId
  mbSchedule <- withTryCatch "FRFSReschedule:getNewTripStartTime" (OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBppConfig)
  case mbSchedule of
    Left err -> do
      logWarning $ "FRFSReschedule:getNewTripStartTime failed to fetch bus trip schedule, allowing reschedule: " <> show err
      pure Nothing
    Right schedule ->
      case concatMap (.eta) schedule of
        [] -> pure Nothing
        allEtas -> do
          let mbBoardingEta = listToMaybe (filter (\e -> e.stopCode == boardingStopCode) allEtas)
              chosenEta = fromMaybe (minimumBy (comparing (.arrivalTimeUnix)) allEtas) mbBoardingEta
          pure $ Just (FRFSUtils.unixToUTC chosenEta.arrivalTimeUnix)

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
withRescheduleLock bookingId action = do
  resultRef <- liftIO $ newIORef Nothing
  Redis.withLockRedis (rescheduleLockKey bookingId) 15 $ do
    result <- action
    liftIO $ writeIORef resultRef (Just result)
  mbResult <- liftIO $ readIORef resultRef
  mbResult & fromMaybeM (InternalError "withRescheduleLock: action did not complete")

completeReschedule ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  Id DFRFSTicketBooking.FRFSTicketBooking ->
  m ()
completeReschedule oldBookingId newBookingId = do
  oldBooking <- QFRFSTicketBooking.findById oldBookingId >>= fromMaybeM (InvalidRequest "Old booking not found while completing reschedule")
  unless (oldBooking.status == DFRFSTicketBookingStatus.RESCHEDULED) $ do
    quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId oldBooking.quoteId
    void $ QTicket.updateAllStatusByBookingId DFRFSTicketStatus.CANCELLED oldBookingId
    FRFSCancel.releaseSeatsIfHeld oldBooking quoteCategories
    void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.RESCHEDULED oldBookingId
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBookingStatus.RESCHEDULED oldBookingId
    logInfo $ "FRFSReschedule:completeReschedule oldBookingId=" <> oldBookingId.getId <> " newBookingId=" <> newBookingId.getId
