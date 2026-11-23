module SharedLogic.FRFSTripCancel
  ( previewTripCancel,
    confirmTripCancel,
    tripCancelStatus,
    listTripCancellations,
  )
where

import qualified API.Types.RiderPlatform.Management.FRFSTicket as API
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Control.Applicative ((<|>))
import Data.List (findIndex, nub, sortOn)
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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RouteStopMapping as DRSM
import Environment
import qualified ExternalBPP.CallAPI.Cancel as CallExternalBPP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.FRFSCancel as FRFSCancel
import qualified SharedLogic.FRFSCancelJourney as FRFSCancelJourney
import qualified SharedLogic.FRFSLiveTrip as FRFSLiveTrip
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import qualified Storage.Queries.EntityValueLog as QEntityValueLog
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Tools.Notifications as Notify

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

tripCancelledPnKey :: Text
tripCancelledPnKey = "FRFS_TRIP_CANCELLED_BY_OPERATOR"

tripEntityType :: Text
tripEntityType = "FRFS_TRIP"

lockKey :: Id DMOC.MerchantOperatingCity -> Text -> Text
lockKey merchantOperatingCityId tripId = "frfsTripCancelLock:" <> merchantOperatingCityId.getId <> ":" <> tripId

lockTtlSeconds :: Int
lockTtlSeconds = 600

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
  unless locked $ throwError (InvalidRequest "A cancellation for this trip is already in progress")
  logEntry <-
    withTryCatch "frfsTripCancel:createLog" (createCancelLog merchantOperatingCity tripId operationId reason CancelRequested {stopCode = req.stopCode, bookingIds = selectedIdTexts})
      >>= either (\err -> Redis.unlockRedis (lockKey merchantOperatingCityId tripId) >> throwError (InternalError $ "Failed to record trip cancellation: " <> show err)) pure
  fork ("frfsTripCancel:" <> tripId) $ do
    results <- forM toCancel $ \c -> do
      result <- withTryCatch "frfsTripCancel:cancelOne" $ cancelOne operatorCancellation c.cbBooking
      case result of
        Left err -> do
          logError $ "FRFS trip cancel failed for bookingId-" <> c.cbBooking.id.getId <> " tripId-" <> tripId <> ": " <> show err
          pure (Just c.cbBooking.id.getId)
        Right () -> pure Nothing
    let failedIds = catMaybes results
        cancelledCount = length toCancel - length failedIds
        outcome = CancelResult {cancelled = cancelledCount, failedBookingIds = failedIds}
    recordOutcome
      logEntry
        { DEntityValueLog.status = if cancelledCount > 0 then DEntityValueLog.APPLIED else DEntityValueLog.FAILED,
          DEntityValueLog.valueAfter = Just (encodeToText outcome)
        }
    Redis.unlockRedis (lockKey merchantOperatingCityId tripId)
  pure API.TripCancelResp {accepted = length toCancel}

recordOutcome :: DEntityValueLog.EntityValueLog -> Flow ()
recordOutcome entry = attempt (1 :: Int)
  where
    attempt n =
      withTryCatch "frfsTripCancel:recordOutcome" (QEntityValueLog.updateByPrimaryKey entry) >>= \case
        Right () -> pure ()
        Left err
          | n < 3 -> attempt (n + 1)
          | otherwise -> logError $ "FRFS trip cancel outcome not recorded for operationId-" <> entry.operationId <> " tripId-" <> entry.entityId <> ": " <> show err <> " outcome: " <> fromMaybe "" entry.valueAfter

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
  let limit = min 100 (fromMaybe 50 mbLimit)
      offset = fromMaybe 0 mbOffset
  map toOperation <$> QEntityValueLog.findAllByEntityTypeAndOperationTypeInCity tripEntityType DEntityValueLog.TripCancel merchantOperatingCityId fromDate toDate limit offset

cancelOne :: DOnCancel.OperatorCancellation -> DFRFSTicketBooking.FRFSTicketBooking -> Flow ()
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
  whenJust mbSideEffectData $ \(mRiderNumber, mRiderMobileCountryCode, fareParameters, updatedBooking) -> do
    void . withTryCatch "frfsTripCancel:sideEffects" $ FRFSCancel.handleCancelledSideEffects updatedBooking mRiderNumber mRiderMobileCountryCode fareParameters
    void . withTryCatch "frfsTripCancel:cancelJourney" $ FRFSCancelJourney.cancelJourneyIfOnlyTransitLeg updatedBooking.searchId.getId
    void . withTryCatch "frfsTripCancel:notify" $ Notify.notifyCancellationConsequence updatedBooking.riderId updatedBooking.id.getId tripCancelledPnKey

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
            pure $ do
              fromIdx <- findIndex (\(s :: DRSM.RouteStopMapping) -> s.stopCode `elem` fromCodes) stops
              toOffset <- findIndex (\(s :: DRSM.RouteStopMapping) -> s.stopCode `elem` toCodes) (drop (fromIdx + 1) stops)
              pure (fromIdx, fromIdx + 1 + toOffset)
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
        mbIndices <- stopIndices routeStops booking.fromStationCode booking.toStationCode
        mbPayment <- QFRFSTicketBookingPayment.findTicketBookingPayment booking
        multimodalOrder <- case mbPayment of
          Nothing -> pure False
          Just payment -> do
            mbOrder <- QPaymentOrder.findById payment.paymentOrderId
            pure (maybe False (\order -> order.paymentServiceType == Just DOrder.FRFSMultiModalBooking) mbOrder)
        let quantity = quantityOf booking
            isPass = booking.overrideType == Just DFRFSTicketBooking.PassOverride
            paymentMode
              | isPass && FRFSPassOverride.fullyCoveredByPass booking = API.PASS
              | isPass = API.PASS_PARTIAL
              | isJust mbPayment = API.ONLINE
              | otherwise = API.FREE
            refundAmount
              | paymentMode `elem` [API.PASS, API.FREE] = 0
              | otherwise = fromMaybe booking.totalPrice.amount booking.overriddenAmount
        passTrips <-
          if isPass
            then do
              mbBenefit <- FRFSPassOverride.benefitForOverrideAppliedEntity booking.overrideAppliedEntityId
              pure $ case mbBenefit of
                Just benefit | not (FRFSPassOverride.isUnlimitedBenefit benefit) -> max 1 quantity
                _ -> 0
            else pure 0
        let mbFromIdx = fst <$> mbIndices
            mbToIdx = snd <$> mbIndices
            mkStop code fallbackName mbIdx =
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
