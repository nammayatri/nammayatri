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
    depositHoldState,
    bookingDepositFulfilLockKey,
    withBookingDepositFulfilLock,
    bookingDepositFulfilTriggeredKey,
    bookingDepositPollSyncLockKey,
    isBookingDepositConfirmTriggered,
    depositAttemptVerdict,
    isDeadDepositOrder,
    isFailedDepositAttempt,
    reserveBookingDeposit,
    rekeyBookingDepositHold,
    decideAndSecureBookingDeposit,
    FeeDecision (..),
    ReserveResult (..),
    hasCreditForOrder,
    captureBookingDeposit,
    releaseHolds,
    refundBookingDeposit,
    prepareDepositRefundLedger,
    executeDepositRefundGateway,
    creditRiderBalance,
    expireOrRepairBookingDeposit,
    holdGraceSeconds,
    unpaidFeeGraceSeconds,
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
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Finance.Account.Service as Account
import Lib.Finance.Domain.Types.Account (CounterpartyType (..))
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import Lib.Finance.FinanceM
import qualified Lib.Finance.Ledger.Service as Ledger
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as QPaymentTransaction
import qualified Lib.Payment.Storage.HistoryQueries.Refunds as HQRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.BookingDepositLedger
import qualified SharedLogic.Finance.RidePayment as RidePayment
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.BookingPayment as QBookingPayment
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide
import Tools.Error

bookingDepositHoldRefType, bookingDepositTopupRefType :: Text
bookingDepositHoldRefType = "BOOKING_DEPOSIT_HOLD"
bookingDepositTopupRefType = "BOOKING_DEPOSIT_TOPUP"

type DepositFlow m r = (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m)

type DepositHoldFlow m r =
  ( DepositFlow m r,
    SchedulerFlow r,
    HasField "schedulerType" r SchedulerType,
    HasField "blackListedJobs" r [Text]
  )

-- | How long after booking.startTime a NEVER-STAFFED booking's fee hold is expired by
--   BookingDepositExpiry. Holds on bookings that actually got a driver never expire on age.
holdGraceSeconds :: Int
holdGraceSeconds = 600

unpaidFeeGraceSeconds :: Int
unpaidFeeGraceSeconds = 1200

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
          (Just [bookingDepositHoldRefType, bookingDepositRefundRefType])
          Nothing
          Nothing
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

rekeyBookingDepositHold ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  DRB.Booking ->
  m ()
rekeyBookingDepositHold oldBooking newBooking =
  whenJust newBooking.bookingDepositAmount $ \fee ->
    withRiderFeeLock oldBooking.riderId $ do
      oldHolds <- findHolds oldBooking.id
      if null oldHolds
        then logError $ "Booking deposit rekey: old booking " <> oldBooking.id.getId <> " holds no deposit; placing none on " <> newBooking.id.getId
        else do
          holdBookingDeposit_ newBooking fee
          voided <- withTryCatch "rekeyBookingDepositHold:releaseOldHold" $ releaseHolds_ oldBooking.id
          case voided of
            Left err -> do
              releaseHolds_ newBooking.id
              throwError . InternalError $ "Booking deposit rekey could not release the old hold for " <> oldBooking.id.getId <> ": " <> show err
            Right () -> pure ()
      -- PENDING and FAILED too: a superseded order can still be CHARGED, and only the new booking is reconciled from here on.
      rows <- filter (\r -> r.status `elem` [DBP.SUCCESS, DBP.PENDING, DBP.FAILED]) <$> QBookingPayment.findAllByBookingIdAndServiceType oldBooking.id DOrder.BookingDeposit
      forM_ rows $ \row -> do
        newRowId <- generateGUID
        now <- getCurrentTime
        QBookingPayment.create
          row{DBP.id = newRowId,
              DBP.bookingId = newBooking.id,
              DBP.updatedAt = now
             }

-- | Whether the fee ended up reserved against this booking.
data ReserveResult = Reserved | Insufficient
  deriving (Eq, Show)

-- | Outcome of the locked secure-or-plan decision.
data FeeDecision = FeeSecured | FeeShortfall HighPrecMoney

-- | Fee is either secured (a live hold already exists, or the balance covers it and the hold is placed right here) or the caller must fund the reported shortfall with a payment order.
decideAndSecureBookingDeposit ::
  DepositHoldFlow m r =>
  DRB.Booking ->
  HighPrecMoney ->
  m (FeeDecision, HighPrecMoney)
decideAndSecureBookingDeposit booking fee =
  withRiderFeeLock booking.riderId $ do
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
  decideAndSecureBookingDeposit booking fee <&> \case
    (FeeSecured, _) -> Reserved
    (FeeShortfall _, _) -> Insufficient

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
    then logInfo $ "Booking deposit already held for booking " <> bookingId.getId <> "; skipping duplicate hold"
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
        Left err -> throwError $ InternalError $ "Booking deposit hold failed: " <> show err
        Right (Nothing, _) -> throwError $ InternalError "Booking deposit hold produced no ledger entry"
        Right (Just entryId, _) ->
          logInfo $ "Held booking deposit " <> show amount <> " entry " <> entryId.getId <> " booking " <> bookingId.getId

bookingDepositFulfilLockKey, bookingDepositFulfilTriggeredKey, bookingDepositPollSyncLockKey :: Text -> Text
bookingDepositFulfilLockKey bookingIdText = "BookingDeposit:Fulfil:" <> bookingIdText
bookingDepositFulfilTriggeredKey bookingIdText = "BookingDeposit:FulfilTriggered:" <> bookingIdText
bookingDepositPollSyncLockKey orderIdText = "BookingDeposit:PollSync:" <> orderIdText

withBookingDepositFulfilLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DRB.Booking -> m a -> m a
withBookingDepositFulfilLock bookingId = Redis.withWaitAndLockRedis (bookingDepositFulfilLockKey bookingId.getId) 60 10000

isBookingDepositConfirmTriggered :: CacheFlow m r => Id DRB.Booking -> m Bool
isBookingDepositConfirmTriggered bookingId =
  (== Just "1") <$> Redis.get @Text (bookingDepositFulfilTriggeredKey bookingId.getId)

depositHoldState :: (CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m (Bool, [LE.LedgerEntry])
depositHoldState bookingId = do
  entries <- Ledger.getEntriesByReference bookingDepositHoldRefType bookingId.getId
  pure (any (\e -> e.status == LE.SETTLED) entries, filter (\e -> e.status == LE.PENDING) entries)

-- | (in flight, failed) for the latest attempt, from one read of its order. In flight: still PENDING on our
--   side but already CHARGED at the gateway. Failed: still PENDING and its last transaction failed; the order
--   stays live, since a retry on it can still be CHARGED, so this is a retryable failure, not an unpaid verdict.
depositAttemptVerdict :: (CacheFlow m r, EsqDBFlow m r) => Maybe DBP.BookingPayment -> m (Bool, Bool)
depositAttemptVerdict = \case
  Just row
    | row.status == DBP.PENDING -> do
      mbOrder <- QPaymentOrder.findById row.paymentOrderId
      let inFlight = maybe False (\o -> o.status == Payment.CHARGED) mbOrder
      pure (inFlight, not inFlight && maybe False (isFailedDepositAttempt . (.status)) mbOrder)
  _ -> pure (False, False)

isDeadDepositOrder :: Payment.TransactionStatus -> Bool
isDeadDepositOrder = (`elem` [Payment.CANCELLED, Payment.CLIENT_AUTH_TOKEN_EXPIRED])

isFailedDepositAttempt :: Payment.TransactionStatus -> Bool
isFailedDepositAttempt = (`elem` [Payment.AUTHENTICATION_FAILED, Payment.AUTHORIZATION_FAILED, Payment.JUSPAY_DECLINED])

findHolds :: (CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m [LE.LedgerEntry]
findHolds bookingId = snd <$> depositHoldState bookingId

captureBookingDeposit ::
  DepositFlow m r =>
  DRB.Booking ->
  m HighPrecMoney
captureBookingDeposit booking =
  if isJust booking.bookingDepositAmount
    then withRiderFeeLock booking.riderId $ settleHolds_ booking.id
    else pure 0

settleHolds_ :: (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r) => Id DRB.Booking -> m HighPrecMoney
settleHolds_ bookingId = do
  entries <- Ledger.getEntriesByReference bookingDepositHoldRefType bookingId.getId
  let pending = filter (\e -> e.status == LE.PENDING) entries
      settled = filter (\e -> e.status == LE.SETTLED) entries
  unless (null pending) $ do
    forM_ pending $ \e -> Ledger.settleEntry e.id
    logInfo $ "Captured " <> show (length pending) <> " booking deposit hold(s) for booking " <> bookingId.getId
  pure $ sum (map (.amount) (pending <> settled))

-- | Release by booking id alone, for the expiry job's orphan case where the booking row was never written and no rider id is to hand.
releaseHolds ::
  DepositFlow m r => Id DRB.Booking -> m ()
releaseHolds bookingId = do
  holds <- findHolds bookingId
  case holds of
    [] -> pure ()
    (entry : _) -> do
      logError $ "Orphan booking deposit hold on booking " <> bookingId.getId <> " released to wallet, not refunded"
      mbAcc <- Account.getAccount entry.fromAccountId
      case mbAcc >>= (.counterpartyId) of
        Just riderId ->
          withRiderFeeLock (Id riderId) $ releaseHolds_ bookingId
        Nothing -> do
          logError $ "Booking deposit hold on booking " <> bookingId.getId <> " has no counterparty on its account; releasing unlocked"
          releaseHolds_ bookingId

releaseHolds_ :: (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r) => Id DRB.Booking -> m ()
releaseHolds_ bookingId = do
  holds <- findHolds bookingId
  unless (null holds) $ do
    forM_ holds $ \e -> Ledger.voidEntry e.id "booking deposit released"
    logInfo $ "Released " <> show (length holds) <> " booking deposit hold(s) for booking " <> bookingId.getId

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
      mbReq <- withDepositRefundLock order.id $ claimDepositRefundAttempt booking pair
      forM_ mbReq $ \reqRow -> executeDepositRefundGateway booking reqRow False pair
    case eRes of
      Left () -> logInfo $ "Deposit refund for order " <> order.id.getId <> " already being processed elsewhere; skipping"
      Right () -> pure ()

-- | Ledger half of a deposit refund, shared by the inline cancel path and the dashboard queue. Capture instead if the booking is COMPLETED, refuse if the deposit was already captured, void the holds, post the refund legs per paid order
prepareDepositRefundLedger ::
  DepositFlow m r =>
  DRB.Booking ->
  m [(DBP.BookingPayment, DOrder.PaymentOrder)]
prepareDepositRefundLedger booking
  | isNothing booking.bookingDepositAmount = pure []
  | otherwise =
    withRiderFeeLock booking.riderId $ do
      bookingNow <- QRB.findById booking.id >>= fromMaybeM (BookingDoesNotExist booking.id.getId)
      when (bookingNow.status == DRB.COMPLETED) $ do
        captured <- settleHolds_ booking.id
        logWarning $ "Refund requested for COMPLETED booking " <> booking.id.getId <> "; captured its deposit (" <> show captured <> ") instead"
      (alreadyCaptured, holds) <- depositHoldState booking.id
      if alreadyCaptured
        then [] <$ logError ("Booking deposit for booking " <> booking.id.getId <> " is already captured; refusing to refund")
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
              -- Only money that entered the wallet may leave through it. An order refunded straight
              -- to source was never credited; refunding it here would spend another order's credit.
              credited <- hasCreditForOrder order.id.getId
              available <- getAvailableBalance booking.riderId
              if not credited
                then Nothing <$ logError ("Booking deposit refund skipped for order " <> order.id.getId <> ": never credited to the wallet, so it is not refundable from it")
                else
                  if available < order.amount
                    then
                      Nothing
                        <$ logError
                          ("Booking deposit refund skipped for order " <> order.id.getId <> ": available balance " <> show available <> " < refund amount " <> show order.amount <> " (credit re-spent on a live booking); ops can retry after that booking settles")
                    else do
                      QBookingPayment.updateStatusById DBP.REFUND_PENDING row.id
                      let ctx = mkCtx booking.riderId booking.merchantId booking.merchantOperatingCityId order.id.getId
                      result <- runFinance ctx $ do
                        void $ transferPending OwnerLiability BuyerExternal order.amount bookingDepositRefundRefType
                        void $ transferPending BuyerExternal BuyerAsset order.amount bookingDepositRefundRefType
                      case result of
                        Left err -> Nothing <$ logError ("Booking deposit refund ledger failed for order " <> order.id.getId <> ": " <> show err)
                        Right _ -> pure (Just (row, order))

-- | One auto-approved refund_request per deposit order, so every refund -- inline or ops-triggered -- is visible and retryable in the dashboard queue. Reuses a live APPROVED row (crash resume); leaves FAILED rows for ops (retry is an explicit /respond decision, never automatic); skips REFUNDED/REJECTED.
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

-- | Run under withDepositRefundLock: returns the request only to the one caller that may call the gateway.
claimDepositRefundAttempt ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasActorInfo m r,
    MonadMask m,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  (DBP.BookingPayment, DOrder.PaymentOrder) ->
  m (Maybe DRefundRequest.RefundRequest)
claimDepositRefundAttempt booking pair@(_, order) = do
  mbReq <- findOrCreateDepositRefundRequest booking pair
  case mbReq of
    Nothing -> pure Nothing
    Just req
      | isJust req.refundsId -> Nothing <$ logInfo ("Deposit refund request " <> req.id.getId <> " already has a gateway attempt; not calling again")
      | otherwise -> do
        linked <- mapMaybe (.refundsId) <$> QRefundRequest.findAllByOrderId order.id
        attempts <- HQRefunds.findAllByOrderId order.shortId
        -- An unlinked attempt made after the request means the caller died before recording it.
        case find (\a -> a.createdAt >= req.createdAt && a.id `notElem` linked) attempts of
          Just attempt -> do
            logError $ "Deposit refund request " <> req.id.getId <> " lost its attempt " <> attempt.id.getId <> "; recording it instead of calling the gateway again"
            applyDepositRefundResult booking req pair attempt.id.getId attempt.status attempt.errorCode
            pure Nothing
          Nothing -> do
            claimed <- claimDepositRefundCall req.id.getId
            if claimed
              then pure (Just req)
              else Nothing <$ logInfo ("Deposit refund for request " <> req.id.getId <> " is being sent elsewhere; skipping")

-- | Calls the gateway without the deposit refund lock, then records the verdict under it unless someone already did.
executeDepositRefundGateway ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasActorInfo m r,
    MonadMask m,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  DRefundRequest.RefundRequest ->
  Bool ->
  (DBP.BookingPayment, DOrder.PaymentOrder) ->
  m ()
executeDepositRefundGateway booking refundReq retryIfFailed pair@(row, order) = do
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
  rider <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbResp <- SPayment.makeRefundPaymentByServiceType booking.merchantId booking.merchantOperatingCityId row.paymentServiceType rider.clientSdkVersion gwReq
  case mbResp of
    Nothing -> logInfo $ "Deposit refund gateway skipped for order " <> order.id.getId <> " (in flight elsewhere or attempt already stands); leaving request APPROVED"
    Just resp -> withDepositRefundLock order.id $ do
      mbReqNow <- QRefundRequest.findById refundReq.id
      case mbReqNow of
        Just reqNow
          | reqNow.status == DRefundRequest.APPROVED && maybe True (== Id resp.refundId) reqNow.refundsId ->
            applyDepositRefundResult booking reqNow pair resp.refundId resp.status resp.errorCode
        _ -> logInfo $ "Deposit refund " <> resp.refundId <> " for order " <> order.id.getId <> " was already recorded; not overwriting it"

-- | Settles or voids the refund legs and records the attempt on the refund request and the booking_payment row.
applyDepositRefundResult ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasActorInfo m r,
    MonadMask m,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasField "blackListedJobs" r [Text]
  ) =>
  DRB.Booking ->
  DRefundRequest.RefundRequest ->
  (DBP.BookingPayment, DOrder.PaymentOrder) ->
  Text ->
  Payment.RefundStatus ->
  Maybe Text ->
  m ()
applyDepositRefundResult booking refundReq (row, order) refundId refundStatus errorCode = do
  riderConfig <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  resolveDepositRefundLegs booking.riderId order.id refundStatus
  let reqStatus = SPayment.refundStatusToRequestStatus refundStatus
      bpStatus = case refundStatus of
        Payment.REFUND_SUCCESS -> DBP.REFUNDED
        Payment.REFUND_FAILURE -> DBP.REFUND_FAILED
        Payment.REFUND_CANCELED -> DBP.REFUND_FAILED
        _ -> DBP.REFUND_INITIATED
  QRefundRequest.updateRefundIdAndStatus (Just (Id refundId)) reqStatus refundReq.id
  QBookingPayment.updateStatusById bpStatus row.id
  createJobIn @_ @'CheckRefundStatus (Just booking.merchantId) (Just booking.merchantOperatingCityId) riderConfig.refundStatusUpdateInterval $
    CheckRefundStatusJobData {refundId = refundId, numberOfRetries = 0}
  logInfo $ "Scheduled CheckRefundStatus for deposit refund " <> refundId <> " order " <> order.id.getId
  when (reqStatus == DRefundRequest.FAILED) $
    logError $
      "Booking deposit gateway refund FAILED for booking " <> booking.id.getId <> " order " <> order.id.getId
        <> " amount "
        <> show order.amount
        <> " (code "
        <> show errorCode
        <> "); refund legs voided, amount back in the rider's wallet. Deposit refund retries are disabled; settle it manually."

-- | Resolve a fee-bearing booking that was never staffed: cancel it BAP-locally and settle the fee
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
  entries <- Ledger.getEntriesByReference bookingDepositHoldRefType booking.id.getId
  let everHeld = not (null entries)
      (graceAnchor, graceSeconds) =
        if everHeld
          then (booking.startTime, holdGraceSeconds)
          else (booking.createdAt, unpaidFeeGraceSeconds)
      repairable =
        isJust booking.bookingDepositAmount
          && booking.status `elem` [DRB.NEW, DRB.CONFIRMED]
          && isNothing mbRide
          && addUTCTime (fromIntegral graceSeconds) graceAnchor < now
  if not repairable
    then do
      logInfo $
        "BookingDepositExpiry: booking " <> booking.id.getId <> " not repairable; deposit=" <> show booking.bookingDepositAmount
          <> " status="
          <> show booking.status
          <> " activeRide="
          <> show (isJust mbRide)
          <> " graceEnd="
          <> show (addUTCTime (fromIntegral graceSeconds) graceAnchor)
          <> " anchoredOn="
          <> (if everHeld then "startTime (ever held)" else "createdAt (never held)")
          <> " now="
          <> show now
      pure False
    else do
      outcome <- withTryCatch "expireOrRepairBookingDeposit:cancellationLock" $ do
        cancelled <- SharedCancel.tryCancellationLock booking.transactionId $ do
          bookingNow <- QRB.findById booking.id >>= fromMaybeM (BookingDoesNotExist booking.id.getId)
          rideNow <- QRide.findActiveByRBId booking.id
          let stillRepairable = bookingNow.status `elem` [DRB.NEW, DRB.CONFIRMED] && isNothing rideNow
          when stillRepairable $ do
            QRB.updateStatus booking.riderId booking.id DRB.CANCELLED
            QBPL.makeAllInactiveByBookingId booking.id
            QBCR.upsert =<< buildLocalCancellationReason booking
          pure stillRepairable
        SharedCancel.releaseCancellationLock booking.transactionId
        pure cancelled
      case outcome of
        Left err -> do
          logInfo $ "Booking deposit repair skipped for " <> booking.id.getId <> "; cancellation in progress elsewhere: " <> show err
          pure False
        Right False -> do
          logInfo $ "Booking deposit repair skipped for " <> booking.id.getId <> "; it was assigned or settled meanwhile"
          pure False
        Right True -> do
          refunded <- withTryCatch "expireOrRepairBookingDeposit:refund" $ refundBookingDeposit booking
          case refunded of
            Left err -> logError $ "Booking " <> booking.id.getId <> " is cancelled but its deposit refund failed; settle it manually: " <> show err
            Right () -> logInfo $ "Repaired never-staffed booking " <> booking.id.getId <> " and settled its booking deposit"
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
        additionalInfo = Just "Booking never staffed past its booking-deposit grace window",
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        riderId = Just booking.riderId,
        createdAt = now,
        updatedAt = now
      }

-- | Money arriving from the payment gateway. Two legs, matching the house pattern: a cash-arrival leg and an allocation leg
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
    logError $ "Booking deposit credit for " <> referenceId <> " has only one leg; ledger is unbalanced"
  if not (null existing)
    then logInfo $ "Booking deposit already credited for " <> referenceId <> "; skipping duplicate credit"
    else do
      let ctx = mkCtx riderId merchantId merchantOpCityId referenceId
      result <- runFinance ctx $ do
        transfer_ BuyerAsset BuyerExternal amount bookingDepositTopupRefType
        transfer BuyerExternal OwnerLiability amount bookingDepositTopupRefType Nothing
      case result of
        Left err -> throwError $ InternalError $ "Booking deposit credit failed: " <> show err
        Right _ -> logInfo $ "Credited " <> show amount <> " to rider " <> riderId.getId
