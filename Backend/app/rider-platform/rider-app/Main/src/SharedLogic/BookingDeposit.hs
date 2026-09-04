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
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
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
import qualified Storage.Queries.Ride as QRide

bookingDepositHoldRefType, bookingDepositTopupRefType, bookingDepositRefundRefType :: Text
bookingDepositHoldRefType = "BOOKING_DEPOSIT_HOLD"
bookingDepositTopupRefType = "BOOKING_DEPOSIT_TOPUP"
bookingDepositRefundRefType = "BOOKING_DEPOSIT_REFUND"

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
    retryDelaysMs = [250, 500, 1000, 2000, 2000, 2000, 2000] :: [Int]
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
--
--   PENDING holds do not reduce account.balance, so they must be subtracted here. This
--   account also carries ride-payment obligations, hence the reference-type filter.
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

-- | Place the hold AND schedule its expiry. These are one operation on purpose: a hold with
--   no expiry job is money with no release path, and there are three call sites (confirm,
--   payment webhook, quote-repetition re-hold). Enqueuing here rather than at each caller
--   makes "hold without release path" unrepresentable.
holdBookingDeposit ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasActorInfo m r,
    SchedulerFlow r,
    MonadMask m,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  HighPrecMoney ->
  m ()
holdBookingDeposit booking amount =
  withRiderFeeLock booking.riderId $ holdBookingDeposit_ booking amount

rekeyBookingDepositHold ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasActorInfo m r,
    SchedulerFlow r,
    MonadMask m,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  DRB.Booking ->
  HighPrecMoney ->
  m ()
rekeyBookingDepositHold oldBooking newBooking fee =
  withRiderFeeLock oldBooking.riderId $ do
    releaseHolds_ oldBooking.id
    holdBookingDeposit_ newBooking fee
    -- Carry only the SUCCESS row (at most one): it is the sole row ever read again -- the
    -- paid-order lookup in refundBookingDeposit. The fee is already secured on the new booking,
    -- so PENDING/FAILED attempt history stays behind as audit on the old one.
    rows <- filter (\r -> r.status == DBP.SUCCESS) <$> QBookingPayment.findAllByBookingIdAndServiceType oldBooking.id DOrder.BookingDeposit
    forM_ rows $ \row -> do
      newRowId <- generateGUID
      now <- getCurrentTime
      QBookingPayment.create
        DBP.BookingPayment
          { id = newRowId,
            bookingId = newBooking.id,
            paymentOrderId = row.paymentOrderId,
            paymentServiceType = row.paymentServiceType,
            status = row.status,
            merchantId = row.merchantId,
            merchantOperatingCityId = row.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

-- | Whether the fee ended up reserved against this booking.
data ReserveResult = Reserved | Insufficient
  deriving (Eq, Show)

-- | Outcome of the locked secure-or-plan decision.
data FeeDecision = FeeSecured | FeeShortfall HighPrecMoney

-- | THE single decision point for funding a fee. Under one rider-lock acquisition: the fee is
--   either secured (a live hold already exists, or the balance covers it and the hold is
--   placed right here) or the caller must fund the reported shortfall with a payment order.
--   A live payment attempt (PENDING or SUCCESS booking_payment row) wins over a balance
--   re-check: the rider may be mid-payment on it, so it must keep being offered until it is
--   paid or expires. (The shortfall reported on that branch is only ever used to build the
--   reused order's request; createOrderService rebuilds an existing order from the stored row
--   and ignores the requested amount.)
decideAndSecureBookingDeposit ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasActorInfo m r,
    SchedulerFlow r,
    MonadMask m,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  HighPrecMoney ->
  m (Maybe (FeeDecision, HighPrecMoney))
decideAndSecureBookingDeposit booking fee =
  withRiderFeeLockV booking.riderId $ do
    existingHolds <- findHolds booking.id
    available <- getAvailableBalance booking.riderId
    sagaInFlight <-
      maybe False (\r -> r.status `elem` [DBP.PENDING, DBP.SUCCESS])
        <$> QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
    let shortfall = fee - available
    if not (null existingHolds)
      then pure (FeeSecured, available)
      else
        if sagaInFlight
          then pure (FeeShortfall shortfall, available)
          else
            if shortfall <= 0
              then (FeeSecured, available) <$ holdBookingDeposit_ booking fee
              else pure (FeeShortfall shortfall, available)

reserveBookingDeposit ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasActorInfo m r,
    SchedulerFlow r,
    MonadMask m,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  HighPrecMoney ->
  m ReserveResult
reserveBookingDeposit booking fee =
  decideAndSecureBookingDeposit booking fee >>= \case
    Just (FeeSecured, _) -> pure Reserved
    Just (FeeShortfall _, _) -> pure Insufficient
    Nothing -> do
      -- Lock wait exhausted: nothing was read and nothing was held. Insufficient degrades to
      -- the pay-before-confirm saga, which re-decides later -- never confirm on a guess.
      logError $ "Booking fee reserve: lock wait exhausted for booking " <> booking.id.getId <> "; treating as not secured"
      pure Insufficient

holdBookingDeposit_ ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    HasActorInfo m r,
    SchedulerFlow r,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  HighPrecMoney ->
  m ()
holdBookingDeposit_ booking amount = do
  let riderId = booking.riderId
      merchantId = booking.merchantId
      merchantOpCityId = booking.merchantOperatingCityId
      bookingId = booking.id
      ctx = mkCtx riderId merchantId merchantOpCityId bookingId.getId
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
        -- transferPending returns Nothing on amount <= 0 or emitLedgerEntries = False. Treating
        -- that as success would confirm a booking with no hold behind it.
        Right (Nothing, _) -> throwError $ InternalError "Booking fee hold produced no ledger entry"
        Right (Just entryId, _) ->
          logInfo $ "Held booking fee " <> show amount <> " entry " <> entryId.getId <> " booking " <> bookingId.getId

findHolds :: (CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m [LE.LedgerEntry]
findHolds bookingId = do
  entries <- Ledger.getEntriesByReference bookingDepositHoldRefType bookingId.getId
  pure $ filter (\e -> e.status == LE.PENDING) entries

captureBookingDeposit ::
  (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m) =>
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

-- | Completed, re-quoted, or cancelled before any driver was assigned: nothing moves and the
--   balance returns to spendable.
releaseBookingDeposit ::
  (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m) => DRB.Booking -> m ()
releaseBookingDeposit booking =
  when (isJust booking.bookingDepositAmount) $
    withRiderFeeLockOrSkip "releaseBookingDeposit" booking.riderId () $ releaseHolds_ booking.id

-- | Release by booking id alone, for the expiry job's orphan case where the booking row was
--   never written and no rider id is to hand.
releaseHolds ::
  (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m) => Id DRB.Booking -> m ()
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
          -- Cannot identify the owner, so the lock cannot be taken. Stranding the hold forever
          -- is the worse outcome, so release anyway and make the anomaly visible.
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
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  m ()
refundBookingDeposit booking = do
  mbGatewayRefund <- withRiderFeeLockOrSkip "refundBookingDeposit" booking.riderId Nothing $ do
    holds <- findHolds booking.id
    if null holds
      then Nothing <$ logInfo ("No live booking fee hold for booking " <> booking.id.getId <> "; nothing to refund")
      else do
        mbPaidRow <-
          find (\r -> r.status == DBP.SUCCESS)
            <$> QBookingPayment.findAllByBookingIdAndServiceType booking.id DOrder.BookingDeposit
        case mbPaidRow of
          Just row -> do
            order <- QPaymentOrder.findById row.paymentOrderId >>= fromMaybeM (InvalidRequest "Booking fee order not found")
            alreadyRefunded <- Ledger.getEntriesByReference bookingDepositRefundRefType booking.id.getId
            if not (null alreadyRefunded)
              then Nothing <$ logInfo ("Booking fee already refunded for booking " <> booking.id.getId <> "; skipping duplicate refund")
              else do
                forM_ holds $ \e -> Ledger.voidEntry e.id "booking fee refunded to source"
                let ctx = mkCtx booking.riderId booking.merchantId booking.merchantOperatingCityId booking.id.getId
                result <- runFinance ctx $ do
                  transfer_ OwnerLiability BuyerExternal order.amount bookingDepositRefundRefType
                  transfer BuyerExternal BuyerAsset order.amount bookingDepositRefundRefType Nothing
                case result of
                  Left err -> throwError $ InternalError $ "Booking fee refund ledger failed: " <> show err
                  Right _ -> pure (Just (row.paymentOrderId, order.amount))
          Nothing -> do
            releaseHolds_ booking.id
            Nothing <$ logInfo ("Booking fee released (no paid order to refund) for " <> booking.id.getId)
  whenJust mbGatewayRefund $ \(orderId, amount) -> do
    void $ SPayment.initiateRefundWithPaymentStatusRespSync booking.riderId orderId
    logInfo $ "Refunded booking fee " <> show amount <> " to source for booking " <> booking.id.getId

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
          && booking.status `notElem` DRB.terminalBookingStatus
          && isNothing mbRide
          && addUTCTime (fromIntegral holdGraceSeconds) booking.startTime < now
  if not repairable
    then pure False
    else do
      SharedCancel.tryCancellationLock booking.transactionId $ do
        refundBookingDeposit booking
        QRB.updateStatus booking.riderId booking.id DRB.CANCELLED
        QBPL.makeAllInactiveByBookingId booking.id
        QBCR.upsert =<< buildLocalCancellationReason booking
      SharedCancel.releaseCancellationLock booking.transactionId
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
--   cash-arrival leg and an allocation leg, with BuyerExternal netting to zero across the
--   pair. Writing only the second leg would never record the cash arriving and would leave
--   BuyerExternal permanently negative.
creditRiderBalance ::
  (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  HighPrecMoney ->
  Text ->
  m ()
creditRiderBalance riderId merchantId merchantOpCityId amount referenceId = withRiderFeeLock riderId $ do
  existing <- Ledger.getEntriesByReference bookingDepositTopupRefType referenceId
  -- runFinance is not a DB transaction, so a half-written pair is possible. Skipping it is
  -- still right -- re-running would duplicate the leg that did land -- but it must be visible.
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
