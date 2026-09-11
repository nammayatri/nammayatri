{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Booking-fee money logic. Every balance read, hold, capture, release and refund for the
--   booking fee goes through this module; callers never touch the ledger directly.
--
--   The model, in one paragraph: the fee is a refundable deposit, not fare. It is held as a
--   PENDING ledger entry against the rider's existing OwnerLiability account, which moves no
--   balances (transferPending calls createEntry, not createEntryWithBalanceUpdate). Spendable
--   balance is therefore @account.balance - sum of PENDING holds@. A hold stops counting only
--   when it stops being PENDING, which only BookingDepositExpiry, the on-read repair
--   (handleConfirmTtlExpiry in Domain.Action.UI.Booking), or a terminal handler can cause.
--   Expiry is deliberately NOT derived on read: an age-based
--   predicate frees money mid-trip (startTime is search time for immediate rides and
--   TRIP_ASSIGNED is not terminal), and an age-plus-never-staffed predicate is non-monotone,
--   so a late driver assignment revives a hold after the money was already re-spent.
module SharedLogic.BookingDeposit
  ( bookingDepositHoldRefType,
    bookingDepositTopupRefType,
    bookingDepositRefundRefType,
    getAvailableBalance,
    findHolds,
    depositCaptured,
    holdBookingDeposit,
    reserveBookingDeposit,
    rekeyBookingDepositHold,
    decideAndSecureBookingDeposit,
    FeeDecision (..),
    ReserveResult (..),
    hasCreditForOrder,
    captureBookingDeposit,
    releaseBookingDeposit,
    releaseHolds,
    refundBookingDeposit,
    prepareDepositRefundLedger,
    executeDepositRefundGateway,
    creditRiderBalance,
    expireOrRepairBookingDeposit,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.SharedLogic.Cancel as SharedCancel
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingPayment as DBP
import qualified Domain.Types.BookingStatus as DRB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RefundRequest as DRefundRequest
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (SchedulerFlow, SchedulerType, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Account.Service as Account
import Lib.Finance.Domain.Types.Account (CounterpartyType (..))
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.FinanceM
import qualified Lib.Finance.Ledger.Service as Ledger
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as QPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.Finance.RidePayment as RidePayment
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.BookingPayment as QBookingPayment
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide

bookingDepositHoldRefType, bookingDepositTopupRefType, bookingDepositRefundRefType :: Text
bookingDepositHoldRefType = "BOOKING_DEPOSIT_HOLD"
bookingDepositTopupRefType = "BOOKING_DEPOSIT_TOPUP"
bookingDepositRefundRefType = "BOOKING_DEPOSIT_REFUND"

type DepositFlow m r = (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m)

-- | Operations that may PLACE a hold, and therefore must be able to schedule its expiry job.
type DepositHoldFlow m r =
  ( DepositFlow m r,
    SchedulerFlow r,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  )

-- | How long after booking.startTime a NEVER-STAFFED booking's fee hold is expired by
--   BookingDepositExpiry. Holds on bookings that actually got a driver never expire on age.
holdGraceSeconds :: Int
holdGraceSeconds = 1200

withRiderFeeLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DP.Person -> m a -> m a
withRiderFeeLock riderId act =
  withRiderFeeLockV riderId act
    >>= fromMaybeM (InternalError $ "Booking fee lock timeout for rider " <> riderId.getId)

withRiderFeeLockOrSkip ::
  (Redis.HedisFlow m r, MonadMask m, MonadFlow m, Log m) => Text -> Id DP.Person -> a -> m a -> m a
withRiderFeeLockOrSkip opName riderId fallback act =
  withRiderFeeLockV riderId act
    >>= \case
      Just a -> pure a
      Nothing -> do
        logError $ "Booking deposit lock timeout for rider " <> riderId.getId <> "; skipped " <> opName
        pure fallback

feeBalanceLockKey :: Id DP.Person -> Text
feeBalanceLockKey riderId = "BookingDeposit:Balance:" <> riderId.getId

withRiderFeeLockV ::
  (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DP.Person -> m a -> m (Maybe a)
withRiderFeeLockV riderId act = Redis.withMasterRedis $ go retryDelaysMs
  where
    key = feeBalanceLockKey riderId
    retryDelaysMs = [250, 500, 500, 1000, 1000, 1500, 2000] :: [Int]
    go delays = do
      gotLock <- Redis.tryLockRedis key 30
      if gotLock
        then Just <$> finally act (Redis.unlockRedis key)
        else case delays of
          [] -> pure Nothing
          (d : rest) -> do
            liftIO $ threadDelay (d * 1000)
            go rest

-- | Spendable balance: account balance minus every PENDING booking-fee hold.
getAvailableBalance ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  m HighPrecMoney
getAvailableBalance riderId = do
  mbAcc <- RidePayment.getWalletAccountByOwner RIDER riderId.getId
  case mbAcc of
    Nothing -> pure 0
    Just acc -> do
      pending <-
        Ledger.findByAccountWithFiltersAndConcernedIndividual
          acc.id
          Nothing
          Nothing
          Nothing
          Nothing
          (Just LE.PENDING)
          (Just [bookingDepositHoldRefType])
          Nothing
      let holds = filter (\e -> e.fromAccountId == acc.id) pending
      pure $ acc.balance - sum (map (.amount) holds)

hasCreditForOrder :: (CacheFlow m r, EsqDBFlow m r) => Text -> m Bool
hasCreditForOrder referenceId = not . null <$> Ledger.getEntriesByReference bookingDepositTopupRefType referenceId

mkCtx :: Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> FinanceCtx
mkCtx riderId merchantId merchantOpCityId referenceId =
  RidePayment.buildRiderFinanceCtx
    merchantId.getId
    merchantOpCityId.getId
    INR
    True
    riderId.getId
    referenceId
    Nothing
    Nothing
    Nothing

-- | Place the hold AND schedule its expiry
holdBookingDeposit ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  HighPrecMoney ->
  m ()
holdBookingDeposit booking amount =
  withRiderFeeLock booking.riderId $ holdBookingDeposit_ booking amount

rekeyBookingDepositHold ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  DRB.Booking ->
  m ()
rekeyBookingDepositHold oldBooking newBooking =
  whenJust newBooking.bookingDepositAmount $ \fee ->
    withRiderFeeLock oldBooking.riderId $ do
      holdBookingDeposit_ newBooking fee
      voided <- withTryCatch "rekeyBookingDepositHold:releaseOldHold" $ releaseHolds_ oldBooking.id
      case voided of
        Left err -> do
          releaseHolds_ newBooking.id
          throwError . InternalError $ "Booking fee rekey could not release the old hold for " <> oldBooking.id.getId <> ": " <> show err
        Right () -> pure ()
      rows <- filter (\r -> r.status == DBP.SUCCESS) <$> QBookingPayment.findAllByBookingIdAndServiceType oldBooking.id DOrder.BookingDeposit
      forM_ rows $ \row -> do
        newRowId <- generateGUID
        now <- getCurrentTime
        QBookingPayment.create
          row{DBP.id = newRowId,
              DBP.bookingId = newBooking.id,
              DBP.createdAt = now,
              DBP.updatedAt = now
             }

-- | Whether the fee ended up reserved against this booking.
data ReserveResult = Reserved | Insufficient
  deriving (Eq, Show)

-- | Outcome of the locked secure-or-plan decision.
data FeeDecision = FeeSecured | FeeShortfall HighPrecMoney

-- | THE single decision point for funding a fee.
--   Fee is either secured (a live hold already exists, or the balance covers it and the hold is
--   placed right here) or the caller must fund the reported shortfall with a payment order.
decideAndSecureBookingDeposit ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  HighPrecMoney ->
  m (Maybe (FeeDecision, HighPrecMoney))
decideAndSecureBookingDeposit booking fee =
  withRiderFeeLockV booking.riderId $ do
    existingHolds <- findHolds booking.id
    available <- getAvailableBalance booking.riderId
    let shortfall = fee - available
    if not (null existingHolds)
      then pure (FeeSecured, available)
      else
        if shortfall <= 0
          then (FeeSecured, available) <$ holdBookingDeposit_ booking fee
          else pure (FeeShortfall shortfall, available)

reserveBookingDeposit ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  HighPrecMoney ->
  m ReserveResult
reserveBookingDeposit booking fee =
  decideAndSecureBookingDeposit booking fee >>= \case
    Just (FeeSecured, _) -> pure Reserved
    Just (FeeShortfall _, _) -> pure Insufficient
    Nothing -> do
      logError $ "Booking fee reserve: lock wait exhausted for booking " <> booking.id.getId <> "; treating as not secured"
      pure Insufficient

holdBookingDeposit_ ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  HighPrecMoney ->
  m ()
holdBookingDeposit_ booking amount = do
  let riderId = booking.riderId
      merchantId = booking.merchantId
      merchantOpCityId = booking.merchantOperatingCityId
      bookingId = booking.id
      ctx = mkCtx riderId merchantId merchantOpCityId bookingId.getId
  bookingNow <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  when (bookingNow.status `elem` DRB.terminalBookingStatus) $
    throwError $ InvalidRequest $ "Booking " <> bookingId.getId <> " is terminal; refusing to place booking fee hold"
  existingHolds <- findHolds bookingId
  if not (null existingHolds)
    then logInfo $ "Booking fee already held for booking " <> bookingId.getId <> "; skipping duplicate hold"
    else do
      now <- getCurrentTime
      let grace = holdGraceSeconds
      let fireIn =
            max
              (fromIntegral grace)
              (diffUTCTime (addUTCTime (fromIntegral grace) booking.startTime) now)
      createJobIn @_ @'BookingDepositExpiry (Just merchantId) (Just merchantOpCityId) fireIn $
        BookingDepositExpiryJobData {bookingId = bookingId}
      result <- runFinance ctx (transferPending OwnerLiability SellerRevenue amount bookingDepositHoldRefType)
      case result of
        Left err -> throwError $ InternalError $ "Booking fee hold failed: " <> show err
        Right (Nothing, _) -> throwError $ InternalError "Booking fee hold produced no ledger entry"
        Right (Just entryId, _) ->
          logInfo $ "Held booking fee " <> show amount <> " entry " <> entryId.getId <> " booking " <> bookingId.getId

-- | Whether any hold for this booking was settled to revenue -- i.e. the deposit was captured
--   (cancellation forfeit or ride completion). A captured deposit is terminal: it can be
--   neither released nor refunded, and the client shows it as FORFEITED.
depositCaptured :: (CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m Bool
depositCaptured bookingId =
  any (\e -> e.status == LE.SETTLED) <$> Ledger.getEntriesByReference bookingDepositHoldRefType bookingId.getId

findHolds :: (CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m [LE.LedgerEntry]
findHolds bookingId = do
  entries <- Ledger.getEntriesByReference bookingDepositHoldRefType bookingId.getId
  pure $ filter (\e -> e.status == LE.PENDING) entries

captureBookingDeposit ::
  DepositFlow m r =>
  DRB.Booking ->
  m HighPrecMoney
captureBookingDeposit booking =
  if isJust booking.bookingDepositAmount
    then withRiderFeeLockOrSkip "captureBookingDeposit" booking.riderId 0 $ settleHolds_ booking.id
    else pure 0

settleHolds_ :: (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r) => Id DRB.Booking -> m HighPrecMoney
settleHolds_ bookingId = do
  entries <- Ledger.getEntriesByReference bookingDepositHoldRefType bookingId.getId
  let pending = filter (\e -> e.status == LE.PENDING) entries
      settled = filter (\e -> e.status == LE.SETTLED) entries
  unless (null pending) $ do
    forM_ pending $ \e -> Ledger.settleEntry e.id
    logInfo $ "Captured " <> show (length pending) <> " booking fee hold(s) for booking " <> bookingId.getId
  pure $ sum (map (.amount) (pending <> settled))

-- | Re-quoted: nothing moves and the balance returns to spendable
releaseBookingDeposit ::
  DepositFlow m r => DRB.Booking -> m ()
releaseBookingDeposit booking =
  when (isJust booking.bookingDepositAmount) $
    withRiderFeeLockOrSkip "releaseBookingDeposit" booking.riderId () $ releaseHolds_ booking.id

-- | Release by booking id alone, for the expiry job's orphan case where the booking row was
--   never written and no rider id is to hand.
releaseHolds ::
  DepositFlow m r => Id DRB.Booking -> m ()
releaseHolds bookingId = do
  holds <- findHolds bookingId
  case holds of
    [] -> pure ()
    (entry : _) -> do
      mbAcc <- Account.getAccount entry.fromAccountId
      case mbAcc >>= (.counterpartyId) of
        Just riderId ->
          withRiderFeeLockOrSkip "releaseHolds" (Id riderId) () $ releaseHolds_ bookingId
        Nothing -> do
          logError $ "Booking fee hold on booking " <> bookingId.getId <> " has no counterparty on its account; releasing unlocked"
          releaseHolds_ bookingId

releaseHolds_ :: (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r) => Id DRB.Booking -> m ()
releaseHolds_ bookingId = do
  holds <- findHolds bookingId
  unless (null holds) $ do
    forM_ holds $ \e -> Ledger.voidEntry e.id "booking fee released"
    logInfo $ "Released " <> show (length holds) <> " booking fee hold(s) for booking " <> bookingId.getId

-- | Platform fault: the rider gets their money back.
refundBookingDeposit ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasActorInfo m r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    MonadMask m,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  m ()
refundBookingDeposit booking = do
  toSettle <- prepareDepositRefundLedger booking
  forM_ toSettle $ \pair@(_, order) -> do
    eRes <- Redis.whenWithLockRedisAndReturnValue (SPayment.refundRequestProccessingKey order.id) 60 $ do
      mbReq <- findOrCreateDepositRefundRequest booking pair
      forM_ mbReq $ \reqRow -> executeDepositRefundGateway booking reqRow False pair
    case eRes of
      Left () -> logInfo $ "Deposit refund for order " <> order.id.getId <> " already being processed elsewhere; skipping"
      Right () -> pure ()

-- | Ledger half of a deposit refund, shared by the inline cancel path and the dashboard
--   queue. Refuse if the deposit was already captured, void the holds, post the refund legs per paid order
prepareDepositRefundLedger ::
  DepositFlow m r =>
  DRB.Booking ->
  m [(DBP.BookingPayment, DOrder.PaymentOrder)]
prepareDepositRefundLedger booking
  | isNothing booking.bookingDepositAmount = pure []
  | otherwise =
    withRiderFeeLockOrSkip "prepareDepositRefundLedger" booking.riderId [] $ do
      entries <- Ledger.getEntriesByReference bookingDepositHoldRefType booking.id.getId
      let holds = filter (\e -> e.status == LE.PENDING) entries
      if any (\e -> e.status == LE.SETTLED) entries
        then -- The deposit was settled to revenue already (cancellation forfeit, or ride completion).
          [] <$ logError ("Booking deposit for booking " <> booking.id.getId <> " is already captured; refusing to refund")
        else do
          rows <-
            filter (\r -> r.status `elem` [DBP.SUCCESS, DBP.REFUND_PENDING, DBP.REFUND_FAILED])
              <$> QBookingPayment.findAllByBookingIdAndServiceType booking.id DOrder.BookingDeposit
          if null rows
            then do
              unless (null holds) $ releaseHolds_ booking.id
              [] <$ logInfo ("Booking deposit released (no paid order to refund) for " <> booking.id.getId)
            else do
              when (length rows > 1) $
                logError $ "Booking " <> booking.id.getId <> " has " <> show (length rows) <> " payable deposit orders; refunding all"
              forM_ holds $ \e -> Ledger.voidEntry e.id "booking deposit refunded to source"
              catMaybes <$> mapM postRefundLegs rows
  where
    postRefundLegs row = do
      mbOrder <- QPaymentOrder.findById row.paymentOrderId
      case mbOrder of
        Nothing -> Nothing <$ logError ("Booking deposit order " <> row.paymentOrderId.getId <> " not found for booking " <> booking.id.getId)
        Just order -> do
          byOrder <- Ledger.getEntriesByReference bookingDepositRefundRefType order.id.getId
          if not (null byOrder)
            then pure (Just (row, order))
            else do
              available <- getAvailableBalance booking.riderId
              if available < order.amount
                then
                  Nothing
                    <$ logError
                      ("Booking deposit refund skipped for order " <> order.id.getId <> ": available balance " <> show available <> " < refund amount " <> show order.amount <> " (credit re-spent on a live booking); ops can retry after that booking settles")
                else do
                  QBookingPayment.updateStatusById DBP.REFUND_PENDING row.id
                  let ctx = mkCtx booking.riderId booking.merchantId booking.merchantOperatingCityId order.id.getId
                  result <- runFinance ctx $ do
                    transfer_ OwnerLiability BuyerExternal order.amount bookingDepositRefundRefType
                    transfer BuyerExternal BuyerAsset order.amount bookingDepositRefundRefType Nothing
                  case result of
                    Left err -> Nothing <$ logError ("Booking deposit refund ledger failed for order " <> order.id.getId <> ": " <> show err)
                    Right _ -> pure (Just (row, order))

-- | One auto-approved refund_request per deposit order, so every refund -- inline or
--   ops-triggered -- is visible and retryable in the dashboard queue. Reuses a live APPROVED
--   row (crash resume); leaves FAILED rows for ops (retry is an explicit /respond decision,
--   never automatic); skips REFUNDED/REJECTED.
findOrCreateDepositRefundRequest ::
  (EsqDBFlow m r, CacheFlow m r) =>
  DRB.Booking ->
  (DBP.BookingPayment, DOrder.PaymentOrder) ->
  m (Maybe DRefundRequest.RefundRequest)
findOrCreateDepositRefundRequest booking (_row, order) = do
  existing <- filter (\r -> r.refundPurpose == DRefundRequest.BOOKING_DEPOSIT) <$> QRefundRequest.findAllByOrderId order.id
  case find (\r -> r.status `elem` [DRefundRequest.OPEN, DRefundRequest.APPROVED]) existing of
    Just live -> pure (Just live)
    Nothing
      | any (\r -> r.status `elem` [DRefundRequest.FAILED, DRefundRequest.REFUNDED]) existing -> do
        logInfo $ "Deposit refund request for order " <> order.id.getId <> " already terminal; not re-filing (ops retry via dashboard)"
        pure Nothing
      | otherwise -> do
        mbTxn <- listToMaybe <$> QPaymentTransaction.findAllByOrderId order.id
        case mbTxn of
          Nothing -> Nothing <$ logError ("Deposit refund: no payment transaction for order " <> order.id.getId <> "; cannot file refund_request")
          Just txn -> do
            reqId <- generateGUID
            now <- getCurrentTime
            let reqRow =
                  DRefundRequest.RefundRequest
                    { id = reqId,
                      orderId = order.id,
                      transactionId = txn.id,
                      transactionAmount = order.amount,
                      currency = order.currency,
                      refundPurpose = DRefundRequest.BOOKING_DEPOSIT,
                      personId = booking.riderId,
                      requestedAmount = Just order.amount,
                      requestedRefundComponents = Nothing,
                      approvedRefundedComponents = Nothing,
                      status = DRefundRequest.APPROVED,
                      code = DRefundRequest.RefundRequestCode "BOOKING_DEPOSIT_REFUND",
                      description = "Booking deposit refund to source (platform fault)",
                      responseDescription = Nothing,
                      evidenceS3Path = Nothing,
                      deductFromDriver = Nothing,
                      refundsAmount = Just order.amount,
                      refundsId = Nothing,
                      refundsTries = 1,
                      merchantId = booking.merchantId,
                      merchantOperatingCityId = booking.merchantOperatingCityId,
                      createdAt = now,
                      updatedAt = now
                    }
            QRefundRequest.create reqRow
            pure (Just reqRow)

-- | refundPaymentService via makeRefundPayment, so a FAILED attempt is
--   retryable (per-attempt refundsId + retryIfFailed). Records the verdict on
--   both the refund_request and the booking_payment row.
executeDepositRefundGateway ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasActorInfo m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  DRB.Booking ->
  DRefundRequest.RefundRequest ->
  Bool ->
  (DBP.BookingPayment, DOrder.PaymentOrder) ->
  m ()
executeDepositRefundGateway booking refundReq retryIfFailed (row, order) = do
  let gwReq =
        DPayment.RefundPaymentServiceReq
          { orderId = order.id,
            merchantOpCityId = cast booking.merchantOperatingCityId,
            driverAccountId = Nothing,
            email = Nothing,
            amount = Just order.amount,
            retryIfFailed = retryIfFailed,
            refundsId = refundReq.refundsId
          }
  refundServiceType <- order.paymentServiceType & fromMaybeM (InvalidRequest $ "Payment service type not found for deposit order: " <> order.id.getId)
  rider <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbResp <- SPayment.makeRefundPaymentByServiceType booking.merchantId booking.merchantOperatingCityId refundServiceType rider.clientSdkVersion gwReq
  case mbResp of
    Nothing -> logInfo $ "Deposit refund gateway skipped for order " <> order.id.getId <> " (in flight elsewhere or attempt already stands); leaving request APPROVED"
    Just resp -> do
      let reqStatus = SPayment.refundStatusToRequestStatus resp.status
          bpStatus = case resp.status of
            Payment.REFUND_SUCCESS -> DBP.REFUNDED
            Payment.REFUND_FAILURE -> DBP.REFUND_FAILED
            Payment.REFUND_CANCELED -> DBP.REFUND_FAILED
            _ -> DBP.REFUND_INITIATED
      QRefundRequest.updateRefundIdAndStatus (Just (Id resp.refundId)) reqStatus refundReq.id
      QBookingPayment.updateStatusById bpStatus row.id
      when (reqStatus == DRefundRequest.FAILED) $
        logError $
          "Booking deposit gateway refund FAILED for booking " <> booking.id.getId <> " order " <> order.id.getId
            <> " amount "
            <> show order.amount
            <> " (code "
            <> show resp.errorCode
            <> "); ledger already refunded, money owed to the rider. Retry from the dashboard refund queue with retryRefunds=true."

-- | Resolve a fee-bearing booking that was never staffed: cancel it BAP-locally and settle
--   the fee. Returns True when it actually repaired something.
expireOrRepairBookingDeposit ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasActorInfo m r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    MonadMask m,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  m Bool
expireOrRepairBookingDeposit booking = do
  now <- getCurrentTime
  mbRide <- QRide.findActiveByRBId booking.id
  let repairable =
        isJust booking.bookingDepositAmount
          && booking.status `elem` [DRB.NEW, DRB.CONFIRMED]
          && isNothing mbRide
          && addUTCTime (fromIntegral holdGraceSeconds) booking.startTime < now
  if not repairable
    then pure False
    else do
      outcome <- withTryCatch "expireOrRepairBookingDeposit:cancellationLock" $ do
        SharedCancel.tryCancellationLock booking.transactionId $ do
          refundBookingDeposit booking
          QRB.updateStatus booking.riderId booking.id DRB.CANCELLED
          QBPL.makeAllInactiveByBookingId booking.id
          QBCR.upsert =<< buildLocalCancellationReason booking
        SharedCancel.releaseCancellationLock booking.transactionId
      case outcome of
        Left err -> do
          logInfo $ "Booking deposit repair skipped for " <> booking.id.getId <> "; cancellation in progress elsewhere: " <> show err
          pure False
        Right _ -> do
          logInfo $ "Repaired never-staffed booking " <> booking.id.getId <> " and settled its booking fee"
          pure True

buildLocalCancellationReason :: MonadFlow m => DRB.Booking -> m SBCR.BookingCancellationReason
buildLocalCancellationReason booking = do
  now <- getCurrentTime
  pure $
    SBCR.BookingCancellationReason
      { bookingId = booking.id,
        rideId = Nothing,
        merchantId = Just booking.merchantId,
        distanceUnit = booking.distanceUnit,
        source = SBCR.ByApplication,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Just "Booking never staffed past its booking-fee grace window",
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        riderId = Just booking.riderId,
        createdAt = now,
        updatedAt = now
      }

-- | Money arriving from the payment gateway. Two legs, matching the house pattern: a
--   cash-arrival leg and an allocation leg
creditRiderBalance ::
  DepositFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  HighPrecMoney ->
  Text ->
  m ()
creditRiderBalance riderId merchantId merchantOpCityId amount referenceId = withRiderFeeLock riderId $ do
  existing <- Ledger.getEntriesByReference bookingDepositTopupRefType referenceId
  when (length existing == 1) $
    logError $ "Booking fee credit for " <> referenceId <> " has only one leg; ledger is unbalanced"
  if not (null existing)
    then logInfo $ "Booking fee already credited for " <> referenceId <> "; skipping duplicate credit"
    else do
      let ctx = mkCtx riderId merchantId merchantOpCityId referenceId
      result <- runFinance ctx $ do
        transfer_ BuyerAsset BuyerExternal amount bookingDepositTopupRefType
        transfer BuyerExternal OwnerLiability amount bookingDepositTopupRefType Nothing
      case result of
        Left err -> throwError $ InternalError $ "Booking fee credit failed: " <> show err
        Right _ -> logInfo $ "Credited " <> show amount <> " to rider " <> riderId.getId
