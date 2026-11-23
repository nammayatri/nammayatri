module SharedLogic.FRFSTripCancel
  ( previewTripCancel,
    confirmTripCancel,
    tripCancelStatus,
    listTripCancellations,
    runTripCancel,
    TripCancelJobFlow,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket as API
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Control.Applicative ((<|>))
import Data.List (findIndex, nub, partition, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Domain.Types.Beckn.FRFS.OnCancel as DOnCancel
import qualified "this" Domain.Types.EntityValueLog as DEntityValueLog
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DTicketStatus
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RouteStopMapping as DRSM
import Environment
import qualified ExternalBPP.CallAPI.Cancel as CallExternalBPP
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.EntityValueLog as SEntityValueLog
import qualified SharedLogic.FRFSCancelJourney as FRFSCancelJourney
import qualified SharedLogic.FRFSLiveTrip as FRFSLiveTrip
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.JobScheduler (FRFSTripCancelJobData (..), RiderJobType (..))
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import qualified Storage.Queries.EntityValueLog as QEntityValueLog
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify
import qualified UrlShortner.Common as UrlShortner

type TripCancelJobFlow m r c =
  ( CacheFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EsqDBFlow m r,
    Finance.HasActorInfo m r,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text],
    HasMasterCloudForwarder r
  )

data ClassifiedBooking = ClassifiedBooking
  { cbBooking :: DFRFSTicketBooking.FRFSTicketBooking,
    cbAction :: API.TripCancelAction,
    cbPaymentMode :: API.TripCancelPaymentMode,
    cbRefundAmount :: HighPrecMoney,
    cbPassTrips :: Int,
    cbQuantity :: Int,
    cbMultimodalOrder :: Bool,
    cbCheckedIn :: Bool,
    cbFromStop :: API.TripCancelStop,
    cbToStop :: API.TripCancelStop,
    cbFromIdx :: Maybe Int,
    cbRefundStatus :: Maybe DFRFSTicketBookingPayment.FRFSTicketBookingPaymentStatus
  }

tripEntityType :: Text
tripEntityType = "FRFS_TRIP"

lockKey :: Id DMOC.MerchantOperatingCity -> Text -> Text
lockKey merchantOperatingCityId tripId = "frfsTripCancelLock:" <> merchantOperatingCityId.getId <> ":" <> tripId

lockTtlSeconds :: Int
lockTtlSeconds = 30

stalePendingAfter :: NominalDiffTime
stalePendingAfter = 1800

toCancelOf :: [ClassifiedBooking] -> [ClassifiedBooking]
toCancelOf = filter ((== API.CANCEL) . (.cbAction))

data CancelRequested = CancelRequested
  { stopCode :: Maybe Text,
    bookingIds :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON)

data CancelResult = CancelResult
  { cancelled :: Int,
    failedBookingIds :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON)

requestedOf :: DEntityValueLog.EntityValueLog -> Maybe CancelRequested
requestedOf entry = entry.valueBefore >>= decodeFromText

resultOf :: DEntityValueLog.EntityValueLog -> Maybe CancelResult
resultOf entry = entry.valueAfter >>= decodeFromText

toOperation :: DEntityValueLog.EntityValueLog -> API.TripCancelOperation
toOperation entry =
  let requested = requestedOf entry
      result = resultOf entry
   in API.TripCancelOperation
        { operationId = entry.operationId,
          tripId = entry.entityId,
          stopCode = requested >>= (.stopCode),
          reason = entry.reason,
          actorId = entry.actorId,
          status = show entry.status,
          requested = maybe 0 (length . (.bookingIds)) requested,
          cancelled = result <&> (.cancelled),
          failed = result <&> (length . (.failedBookingIds)),
          createdAt = entry.createdAt
        }

tripCancelLogs :: Id DMOC.MerchantOperatingCity -> Text -> Flow [DEntityValueLog.EntityValueLog]
tripCancelLogs merchantOperatingCityId tripId =
  sortOn (Down . (.createdAt))
    . filter (\entry -> entry.operationType == DEntityValueLog.TripCancel && entry.merchantOperatingCityId == merchantOperatingCityId)
    <$> QEntityValueLog.findAllByEntityTypeAndEntityId tripEntityType tripId

previewTripCancel :: Id DMOC.MerchantOperatingCity -> Text -> Maybe Text -> Flow API.TripCancelPreviewResp
previewTripCancel merchantOperatingCityId tripId mbStopCode = do
  (cutoffStop, classified) <- classifyTrip ConfirmedOnly merchantOperatingCityId tripId mbStopCode
  logs <- tripCancelLogs merchantOperatingCityId tripId
  let toCancel = toCancelOf classified
  pure $
    API.TripCancelPreviewResp
      { tripId = tripId,
        cutoffStop = cutoffStop,
        summary =
          API.TripCancelSummary
            { totalBookings = length classified,
              toCancel = length toCancel,
              passengersToCancel = sum (map (.cbQuantity) toCancel),
              totalRefundAmount = sum (map (.cbRefundAmount) toCancel),
              passTripsCredited = sum (map (.cbPassTrips) toCancel),
              checkedInToCancel = length (filter (.cbCheckedIn) toCancel),
              notAffected = length (filter ((== API.NOT_AFFECTED) . (.cbAction)) classified)
            },
        stopBreakdown = mkStopBreakdown toCancel,
        bookings = map toBookingPreview classified,
        cancellations = map toOperation logs
      }

confirmTripCancel :: Id DMOC.MerchantOperatingCity -> Text -> API.TripCancelReq -> Flow API.TripCancelResp
confirmTripCancel merchantOperatingCityId tripId req = do
  let reason = T.strip req.reason
  when (T.null reason) $ throwError (InvalidRequest "Cancellation reason is required")
  let selectedIdTexts = nub (map (.getId) req.bookingIds)
  when (null selectedIdTexts) $ throwError (InvalidRequest "Select at least one booking to cancel")
  (_, classified) <- classifyTrip ConfirmedOnly merchantOperatingCityId tripId req.stopCode
  let cancellableIdTexts = map (.cbBooking.id.getId) (toCancelOf classified)
      invalidIdTexts = filter (`notElem` cancellableIdTexts) selectedIdTexts
      toCancel = filter (\c -> c.cbBooking.id.getId `elem` selectedIdTexts) classified
  unless (null invalidIdTexts) $
    throwError . InvalidRequest $
      "Bookings no longer cancellable on this trip, preview again: " <> show invalidIdTexts
  merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  operationId <- generateGUID
  let operatorCancellation = DOnCancel.OperatorCancellation {reason = reason, operationId = operationId}
  blocked <- fmap catMaybes . forM toCancel $ \c -> do
    result <- withTryCatch "frfsTripCancel:gates" $ CallExternalBPP.ensureCancellationAllowed (CallExternalBPP.OperatorInitiated operatorCancellation) merchantOperatingCity c.cbBooking
    pure $ either (const (Just c.cbBooking.id.getId)) (const Nothing) result
  unless (null blocked) $
    throwError . InvalidRequest $
      "Cancellation is not allowed for these bookings by city, provider or service tier config: " <> show blocked
  locked <- Redis.tryLockRedis (lockKey merchantOperatingCityId tripId) lockTtlSeconds
  unless locked $ throwError (InvalidRequest "A cancellation for this trip is already in progress or could not be started, try again")
  flip finally (Redis.unlockRedis (lockKey merchantOperatingCityId tripId)) $ do
    pending <- QEntityValueLog.findPendingByEntityTypeAndEntityId merchantOperatingCityId tripEntityType tripId DEntityValueLog.TripCancel
    now <- getCurrentTime
    let (stale, active) = partition (\entry -> diffUTCTime now entry.createdAt > stalePendingAfter) pending
    forM_ stale $ \entry -> do
      logError $ "FRFS trip cancel log stuck in PENDING, marking FAILED: operationId-" <> entry.operationId <> " tripId-" <> tripId
      QEntityValueLog.updateByPrimaryKey entry {DEntityValueLog.status = DEntityValueLog.FAILED}
    unless (null active) $ throwError (InvalidRequest "A cancellation for this trip is already in progress")
    logEntry <- createCancelLog merchantOperatingCity tripId operationId reason CancelRequested {stopCode = req.stopCode, bookingIds = selectedIdTexts}
    withTryCatch "frfsTripCancel:enqueue" (createJobIn @_ @'FRFSTripCancel (Just merchantOperatingCity.merchantId) (Just merchantOperatingCity.id) 0 FRFSTripCancelJobData {entityValueLogId = logEntry.id.getId}) >>= \case
      Right _ -> pure ()
      Left err -> do
        void . withTryCatch "frfsTripCancel:enqueueFailed" $
          QEntityValueLog.updateByPrimaryKey logEntry {DEntityValueLog.status = DEntityValueLog.FAILED}
        throwError (InternalError $ "Could not schedule the trip cancellation: " <> show err)
  pure API.TripCancelResp {accepted = length toCancel}

runTripCancel :: TripCancelJobFlow m r c => Id DEntityValueLog.EntityValueLog -> m ()
runTripCancel logId = do
  mbEntry <- QEntityValueLog.findByPrimaryKey logId
  case mbEntry of
    Nothing -> logError $ "FRFS trip cancel log not found: " <> logId.getId
    Just entry
      | entry.status /= DEntityValueLog.PENDING -> logInfo $ "FRFS trip cancel already finished: " <> entry.operationId
      | otherwise -> do
        let requestedIds = maybe [] (.bookingIds) (requestedOf entry)
            operatorCancellation = DOnCancel.OperatorCancellation {reason = fromMaybe "" entry.reason, operationId = entry.operationId}
        results <- forM requestedIds $ \bookingIdText -> do
          owned <- logStillPending logId
          if not owned
            then pure (Just bookingIdText)
            else do
              result <- withTryCatch "frfsTripCancel:cancelOne" $ cancelBookingOnce operatorCancellation (Id bookingIdText)
              case result of
                Right True -> pure Nothing
                Right False -> pure (Just bookingIdText)
                Left err -> do
                  logError $ "FRFS trip cancel failed for bookingId-" <> bookingIdText <> " tripId-" <> entry.entityId <> ": " <> show err
                  pure (Just bookingIdText)
        let failedIds = catMaybes results
            cancelledCount = length requestedIds - length failedIds
            outcome = CancelResult {cancelled = cancelledCount, failedBookingIds = failedIds}
        owned <- logStillPending logId
        if owned
          then
            recordOutcome
              entry
                { DEntityValueLog.status = if cancelledCount > 0 then DEntityValueLog.APPLIED else DEntityValueLog.FAILED,
                  DEntityValueLog.valueAfter = Just (encodeToText outcome)
                }
          else logError $ "FRFS trip cancel log no longer PENDING, outcome not recorded for operationId-" <> entry.operationId <> " tripId-" <> entry.entityId <> " outcome: " <> encodeToText outcome

logStillPending :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DEntityValueLog.EntityValueLog -> m Bool
logStillPending logId = (== Just DEntityValueLog.PENDING) . fmap (.status) <$> QEntityValueLog.findByPrimaryKey logId

cancelBookingOnce :: TripCancelJobFlow m r c => DOnCancel.OperatorCancellation -> Id DFRFSTicketBooking.FRFSTicketBooking -> m Bool
cancelBookingOnce operatorCancellation bookingId = do
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest $ "Booking not found: " <> bookingId.getId)
  case booking.status of
    DBookingStatus.CONFIRMED -> cancelOne operatorCancellation booking >> pure True
    DBookingStatus.COUNTER_CANCELLED | booking.cancellationOperationId == Just operatorCancellation.operationId -> pure True
    status -> do
      logInfo $ "FRFS trip cancel skipped bookingId-" <> bookingId.getId <> " status-" <> show status
      pure False

recordOutcome :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DEntityValueLog.EntityValueLog -> m ()
recordOutcome entry = attempt (1 :: Int)
  where
    attempt n =
      withTryCatch "frfsTripCancel:recordOutcome" (QEntityValueLog.updateByPrimaryKey entry) >>= \case
        Right () -> pure ()
        Left err
          | n < 3 -> attempt (n + 1)
          | otherwise -> throwError . InternalError $ "FRFS trip cancel outcome not recorded for operationId-" <> entry.operationId <> " tripId-" <> entry.entityId <> ": " <> show err <> " outcome: " <> fromMaybe "" entry.valueAfter

createCancelLog :: DMOC.MerchantOperatingCity -> Text -> Text -> Text -> CancelRequested -> Flow DEntityValueLog.EntityValueLog
createCancelLog merchantOperatingCity tripId operationId reason requested = do
  logId <- generateGUID
  now <- getCurrentTime
  actorInfo <- asks (.actorInfo)
  let logEntry =
        DEntityValueLog.EntityValueLog
          { id = logId,
            entityType = tripEntityType,
            entityId = tripId,
            operationId = operationId,
            operationType = DEntityValueLog.TripCancel,
            status = DEntityValueLog.PENDING,
            delta = Just (length requested.bookingIds),
            valueBefore = Just (encodeToText requested),
            valueAfter = Nothing,
            reason = Just reason,
            actorType = actorInfo.actorType,
            actorId = actorInfo.actorId,
            merchantId = merchantOperatingCity.merchantId,
            merchantOperatingCityId = merchantOperatingCity.id,
            createdAt = now,
            updatedAt = now
          }
  QEntityValueLog.create logEntry
  pure logEntry

tripCancelStatus :: Id DMOC.MerchantOperatingCity -> Text -> Flow API.TripCancelStatusResp
tripCancelStatus merchantOperatingCityId tripId = do
  logs <- tripCancelLogs merchantOperatingCityId tripId
  (_, classified) <- classifyTrip AllStatuses merchantOperatingCityId tripId Nothing
  let operationIds = map (.operationId) logs
      requestedIds = concatMap (maybe [] (.bookingIds) . requestedOf) logs
      operatorCancelled =
        filter
          (\c -> c.cbBooking.status == DBookingStatus.COUNTER_CANCELLED && maybe False (`elem` operationIds) c.cbBooking.cancellationOperationId)
          classified
      stillPending = filter (\c -> c.cbBooking.status == DBookingStatus.CONFIRMED && c.cbBooking.id.getId `elem` requestedIds) classified
      hasRefundStatus statuses c = maybe False (`elem` statuses) c.cbRefundStatus
  pure $
    API.TripCancelStatusResp
      { tripId = tripId,
        cancellations = map toOperation logs,
        cancelled = length operatorCancelled,
        refundPending = length (filter (hasRefundStatus [DFRFSTicketBookingPayment.REFUND_PENDING, DFRFSTicketBookingPayment.REFUND_INITIATED]) operatorCancelled),
        refunded = length (filter (hasRefundStatus [DFRFSTicketBookingPayment.REFUNDED]) operatorCancelled),
        refundFailed = length (filter (hasRefundStatus [DFRFSTicketBookingPayment.REFUND_FAILED]) operatorCancelled),
        stillPending = length stillPending,
        bookings = map toBookingStatus (operatorCancelled <> stillPending)
      }

listTripCancellations :: Id DMOC.MerchantOperatingCity -> UTCTime -> UTCTime -> Maybe Int -> Maybe Int -> Flow [API.TripCancelOperation]
listTripCancellations merchantOperatingCityId fromDate toDate mbLimit mbOffset = do
  unless (fromDate < toDate) $ throwError (InvalidRequest "fromDate must be before toDate")
  (limit, offset) <- SEntityValueLog.boundedPage mbLimit mbOffset
  map toOperation <$> QEntityValueLog.findAllByEntityTypeAndOperationTypeInCity tripEntityType DEntityValueLog.TripCancel merchantOperatingCityId fromDate toDate limit offset

cancelOne :: TripCancelJobFlow m r c => DOnCancel.OperatorCancellation -> DFRFSTicketBooking.FRFSTicketBooking -> m ()
cancelOne operatorCancellation booking = do
  merchant :: DMerchant.Merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantDoesNotExist booking.merchantId.getId)
  merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  let becknVehicleCategory = Utils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType
  bapConfig <-
    getOneConfig
      (BecknConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, merchantId = merchant.id.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just becknVehicleCategory, becknProtocol = Nothing})
      (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) becknVehicleCategory))
      >>= fromMaybeM (InternalError "Beckn Config not found")
  now <- getCurrentTime
  let decision =
        FRFSLiveTrip.LiveTripDecision
          { canCancel = True,
            cancelDenyReason = Nothing,
            fullRefund = True,
            chargeAnchor = now,
            canReschedule = False,
            rescheduleDenyReason = Nothing
          }
  mbSideEffectData <- CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL (CallExternalBPP.OperatorInitiated operatorCancellation) False (Just decision) Nothing booking
  whenJust mbSideEffectData $ \(_, _, _, updatedBooking) -> do
    void . withTryCatch "frfsTripCancel:cancelJourney" $ FRFSCancelJourney.cancelJourneyIfOnlyTransitLeg updatedBooking.searchId.getId
    let (pnKey, whatsappKey) = operatorCancelMessageKeys (bookingPaymentMode updatedBooking)
    void . withTryCatch "frfsTripCancel:notify" $ Notify.notifyFrfsTripCancelledByOperator updatedBooking.riderId updatedBooking.id.getId pnKey whatsappKey updatedBooking.fromStationName updatedBooking.toStationName

bookingPaymentMode :: DFRFSTicketBooking.FRFSTicketBooking -> API.TripCancelPaymentMode
bookingPaymentMode booking
  | isPass && FRFSPassOverride.fullyCoveredByPass booking = API.PASS
  | isPass = API.PASS_PARTIAL
  | otherwise = API.ONLINE
  where
    isPass = booking.overrideType == Just DFRFSTicketBooking.PassOverride

operatorCancelMessageKeys :: API.TripCancelPaymentMode -> (Text, DMM.MessageKey)
operatorCancelMessageKeys = \case
  API.ONLINE -> ("FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE", DMM.WHATSAPP_FRFS_TRIP_CANCELLED_BY_OPERATOR_ONLINE)
  API.PASS -> ("FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS", DMM.WHATSAPP_FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS)
  API.PASS_PARTIAL -> ("FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL", DMM.WHATSAPP_FRFS_TRIP_CANCELLED_BY_OPERATOR_PASS_PARTIAL)

data BookingScope = ConfirmedOnly | AllStatuses

classifyTrip :: BookingScope -> Id DMOC.MerchantOperatingCity -> Text -> Maybe Text -> Flow (Maybe API.TripCancelStop, [ClassifiedBooking])
classifyTrip scope merchantOperatingCityId tripId mbStopCode = do
  tripBookings <- case scope of
    ConfirmedOnly -> QFRFSTicketBooking.findAllConfirmedByTripId tripId
    AllStatuses -> QFRFSTicketBooking.findAllByTripId tripId
  let bookings = filter (\b -> b.merchantOperatingCityId == merchantOperatingCityId) tripBookings
  case bookings of
    [] -> pure (Nothing, [])
    firstBooking : _ -> do
      integratedBPPConfig <- SIBC.findIntegratedBPPConfigFromEntity firstBooking
      let routeCodes = nub (mapMaybe (.routeCode) bookings)
      routeStopsByRoute <-
        Map.fromList
          <$> forM routeCodes (\routeCode -> (routeCode,) . sortOn (\(s :: DRSM.RouteStopMapping) -> s.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig)
      let equivalentCodes code = (code :) <$> OTPRest.getEquivalentStopCodes code integratedBPPConfig
          stopIndices stops fromCode toCode = do
            fromCodes <- equivalentCodes fromCode
            toCodes <- equivalentCodes toCode
            let isTo (s :: DRSM.RouteStopMapping) = s.stopCode `elem` toCodes
                mbFromIdx = findIndex (\(s :: DRSM.RouteStopMapping) -> s.stopCode `elem` fromCodes) stops
                mbToIdx = case mbFromIdx of
                  Just fromIdx -> (fromIdx + 1 +) <$> findIndex isTo (drop (fromIdx + 1) stops)
                  Nothing -> findIndex isTo stops
            pure (mbFromIdx, mbToIdx)
      cutoffByRoute <- case mbStopCode of
        Nothing -> pure Map.empty
        Just stopCode -> do
          cutoffCodes <- equivalentCodes stopCode
          let cutoffs = Map.mapMaybe (findIndex (\s -> s.stopCode `elem` cutoffCodes)) routeStopsByRoute
          when (Map.null cutoffs) $ throwError (InvalidRequest $ "Stop " <> stopCode <> " is not on this trip's route")
          pure cutoffs
      let cutoffStop = do
            stopCode <- mbStopCode
            routeCode <- firstBooking.routeCode
            idx <- Map.lookup routeCode cutoffByRoute
            stop <- Map.lookup routeCode routeStopsByRoute >>= (`nth` idx)
            pure API.TripCancelStop {code = stopCode, name = Just stop.stopName, seq = Just stop.sequenceNum}
      let bookingIds = map (.id) bookings
      tickets :: [DFRFSTicket.FRFSTicket] <- QFRFSTicket.findAllByTicketBookingIds bookingIds
      let usedBookingIds = nub [t.frfsTicketBookingId | t <- tickets, t.status == DTicketStatus.USED]
      quantityOf <- FRFSUtils.bookedQuantityLookup bookings
      classified <- forM bookings $ \booking -> do
        let routeStops = fromMaybe [] (booking.routeCode >>= (`Map.lookup` routeStopsByRoute))
        (mbFromIdx, mbToIdx) <- stopIndices routeStops booking.fromStationCode booking.toStationCode
        mbPayment <- QFRFSTicketBookingPayment.findTicketBookingPayment booking
        multimodalOrder <- case mbPayment of
          Nothing -> pure False
          Just payment -> do
            mbOrder <- QPaymentOrder.findById payment.paymentOrderId
            pure (maybe False (\order -> order.paymentServiceType == Just DOrder.FRFSMultiModalBooking) mbOrder)
        let quantity = quantityOf booking
            isPass = booking.overrideType == Just DFRFSTicketBooking.PassOverride
            paymentMode = bookingPaymentMode booking
            refundAmount
              | paymentMode == API.PASS || isNothing mbPayment = 0
              | otherwise = fromMaybe booking.totalPrice.amount booking.overriddenAmount
        passTrips <-
          if isPass
            then do
              mbBenefit <- FRFSPassOverride.benefitForOverrideAppliedEntity booking.overrideAppliedEntityId
              pure $ case mbBenefit of
                Just benefit | not (FRFSPassOverride.isUnlimitedBenefit benefit) -> max 1 quantity
                _ -> 0
            else pure 0
        let mkStop code fallbackName mbIdx =
              let mbStop = mbIdx >>= nth routeStops
               in API.TripCancelStop {code = code, name = (mbStop <&> (.stopName)) <|> fallbackName, seq = mbStop <&> (.sequenceNum)}
            checkedIn = booking.id `elem` usedBookingIds || booking.finalBoardedVehicleNumberSource == Just DJourneyLeg.UserActivated
            mbCutoff = booking.routeCode >>= (`Map.lookup` cutoffByRoute)
            action
              | Just cutoff <- mbCutoff, Just toIdx <- mbToIdx, toIdx < cutoff = API.NOT_AFFECTED
              | otherwise = API.CANCEL
        pure
          ClassifiedBooking
            { cbBooking = booking,
              cbAction = action,
              cbPaymentMode = paymentMode,
              cbRefundAmount = refundAmount,
              cbPassTrips = passTrips,
              cbQuantity = quantity,
              cbMultimodalOrder = multimodalOrder,
              cbCheckedIn = checkedIn,
              cbFromStop = mkStop booking.fromStationCode booking.fromStationName mbFromIdx,
              cbToStop = mkStop booking.toStationCode booking.toStationName mbToIdx,
              cbFromIdx = mbFromIdx,
              cbRefundStatus = mbPayment <&> (.status)
            }
      pure (cutoffStop, sortOn (\c -> (fromMaybe maxBound c.cbFromIdx, c.cbBooking.id.getId)) classified)

mkStopBreakdown :: [ClassifiedBooking] -> [API.TripCancelStopBreakdown]
mkStopBreakdown toCancel =
  let grouped = Map.fromListWith (flip (<>)) [(c.cbFromStop.code, [c]) | c <- toCancel]
      ordered = sortOn (\cs -> minimum (map (fromMaybe maxBound . (.cbFromIdx)) cs)) (Map.elems grouped)
   in flip mapMaybe ordered $ \case
        [] -> Nothing
        cs@(c : _) ->
          Just
            API.TripCancelStopBreakdown
              { stop = c.cbFromStop,
                bookings = length cs,
                passengers = sum (map (.cbQuantity) cs),
                refundAmount = sum (map (.cbRefundAmount) cs),
                passTripsCredited = sum (map (.cbPassTrips) cs)
              }

toBookingPreview :: ClassifiedBooking -> API.TripCancelBookingPreview
toBookingPreview c =
  API.TripCancelBookingPreview
    { bookingId = cast c.cbBooking.id,
      riderId = c.cbBooking.riderId.getId,
      fromStop = c.cbFromStop,
      toStop = c.cbToStop,
      quantity = c.cbQuantity,
      paymentMode = c.cbPaymentMode,
      refundAmount = c.cbRefundAmount,
      passTripsCredited = c.cbPassTrips,
      multimodalOrder = c.cbMultimodalOrder,
      checkedIn = c.cbCheckedIn,
      action = c.cbAction
    }

toBookingStatus :: ClassifiedBooking -> API.TripCancelBookingStatus
toBookingStatus c =
  API.TripCancelBookingStatus
    { bookingId = cast c.cbBooking.id,
      bookingStatus = show c.cbBooking.status,
      refundStatus = show <$> c.cbRefundStatus,
      refundAmount = abs <$> c.cbBooking.refundAmount,
      passTripsCredited = if c.cbBooking.status == DBookingStatus.COUNTER_CANCELLED then c.cbPassTrips else 0,
      fromStop = c.cbFromStop,
      toStop = c.cbToStop,
      quantity = c.cbQuantity
    }

nth :: [a] -> Int -> Maybe a
nth xs i
  | i < 0 = Nothing
  | otherwise = listToMaybe (drop i xs)
