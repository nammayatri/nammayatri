module SharedLogic.FRFSCancel where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as FRFSUtils
import qualified Data.HashMap.Strict as HashMap
import Data.List (nub)
import qualified Domain.Action.Beckn.FRFS.GWLink as GWLink
import qualified Domain.Action.Beckn.FRFS.GWLink as GWSA
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSQuoteCategory as FRFSQuoteCategory
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Domain.Types.Merchant as Merchant
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.FRFSSeatBooking as SeatBooking
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.Person as CQP
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QTBP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS
import Tools.Error
import qualified Tools.Metrics as Metrics
import qualified Tools.SMS as Sms
import qualified UrlShortner.Common as UrlShortner
import qualified Utils.Common.JWT.Config as GW
import qualified Utils.Common.JWT.TransitClaim as TC

handleCancelledStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    SchedulerFlow r,
    CallFRFSBPP.BecknAPICallFlow m r,
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text],
    HasShortDurationRetryCfg r c,
    HasLongDurationRetryCfg r c,
    Finance.HasActorInfo m r
  ) =>
  Merchant.Merchant ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  HighPrecMoney ->
  HighPrecMoney ->
  Text ->
  Bool ->
  m (Maybe Text, Maybe Text, FRFSUtils.FRFSFareParameters)
handleCancelledStatus _merchant booking refundAmount cancellationCharges messageId counterCancellationPossible = do
  person <- runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  mbPaymentBooking <- QTBP.findTicketBookingPayment booking
  unless (isJust mbPaymentBooking || FRFSPassOverride.isFullyPassCovered booking.overriddenAmount) $
    throwError (InvalidRequest "Payment booking not found for approved TicketBookingId")
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
  let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
  mRiderNumber <- mapM decrypt person.mobileNumber
  val :: Maybe Text <- Redis.get (FRFSUtils.makecancelledTtlKey booking.id)
  -- The callers hand us a booking read from a replica (see OnUpdate.validateRequest), so its status
  -- can lag a CANCELLED write an earlier callback already made. Every guard below decides whether to
  -- re-run something non-idempotent -- ticket-count decrement, settlement rewrite, trip refund -- so
  -- they have to test master. booking is still used for its immutable fields.
  currentStatus <- maybe booking.status (.status) <$> QTBooking.findById booking.id
  fullyCancelled <-
    if currentStatus == DFRFSTicketBooking.CANCELLED
      then pure False
      else
        if val /= Just messageId && counterCancellationPossible
          then do
            void $ QTBooking.updateStatusById DFRFSTicketBooking.COUNTER_CANCELLED booking.id
            void $ QTicket.updateAllStatusByBookingId DFRFSTicket.COUNTER_CANCELLED booking.id
            void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.COUNTER_CANCELLED) booking.id
            return False
          else do
            void $ checkRefundAndCancellationCharges booking.id refundAmount cancellationCharges
            void $ QTBooking.updateStatusById DFRFSTicketBooking.CANCELLED booking.id
            void $ QTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED booking.id
            void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) booking.id
            void $ QTBooking.updateIsBookingCancellableByBookingId (Just True) booking.id
            void $ QTBooking.updateCustomerCancelledByBookingId True booking.id
            void $ Redis.del (FRFSUtils.makecancelledTtlKey booking.id)
            whenJust mbPaymentBooking $ \paymentBooking ->
              void $ SPayment.markRefundPendingAndSyncOrderStatus booking.merchantId booking.riderId paymentBooking.paymentOrderId
            return True
  -- Refund only if the booking ever reached CONFIRMED, since that is where OnConfirm debits. A
  -- cancel arriving for a booking that never confirmed must not be credited a trip it never spent,
  -- and that is reachable: OnConfirm's expiry path and onConfirmFailure both fire a Technical
  -- CONFIRM_CANCEL on bookings they have just marked FAILED, and the on_cancel lands here.
  --
  -- Stated as the pre-confirm statuses rather than a CONFIRMED allow-list, because the booking has
  -- usually moved on by the time the callback arrives: OnCancel/Core sets CANCEL_INITIATED and
  -- OnStatus sets COUNTER_CANCELLED, both from CONFIRMED and both still owing the rider a trip.
  --
  -- Deliberately OUTSIDE the replay guard below, unlike every other effect here, so that a credit
  -- which failed on a transient Redis or DB error gets retried -- inside the guard the CANCELLED
  -- write has already happened, so nothing would ever come back for it.
  --
  -- On a replay the retry is allowed ONLY when a pending marker says the earlier attempt failed.
  -- The TripReleased marker cannot carry that on its own: it is TTL-bounded, so its absence means
  -- either "never refunded" or "refunded, marker long expired", and treating the second as the
  -- first credits the pass twice for one cancellation.
  refundOwed <-
    if fullyCancelled
      then pure True
      else
        if currentStatus == DFRFSTicketBooking.CANCELLED
          then FRFSPassOverride.hasPendingTripRefund booking.searchId
          else pure False
  when (refundOwed && currentStatus `notElem` [DFRFSTicketBooking.NEW, DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING, DFRFSTicketBooking.CONFIRMING, DFRFSTicketBooking.FAILED]) $
    whenJust booking.overrideAppliedEntityId $ \entityId ->
      -- One trip per ticket went out at confirm, so the same number comes back here.
      void $ withTryCatch "FRFSCancel:refundPassOverrideTrip" (FRFSPassOverride.refundPassOverrideTrip booking.searchId (Id entityId) fareParameters.totalQuantity)

  -- A replayed cancel callback must stop here. The branch above already declines to re-cancel an
  -- CANCELLED booking, but the effects below are not idempotent: the ticket count would be
  -- decremented again and settlement rewritten on every duplicate callback.
  unless (currentStatus == DFRFSTicketBooking.CANCELLED) $ do
    releaseSeatsIfHeld booking quoteCategories
    void $ QPS.incrementTicketsBookedInEvent booking.riderId (- (fareParameters.totalQuantity))
    void $ CQP.clearPSCache booking.riderId
    bapConfig <-
      getOneConfig (BecknConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId, merchantId = booking.merchantId.getId, domain = Just (show Spec.FRFS), vehicleCategory = Just (FRFSUtils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType)}) (Just (maybeToList <$> CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback booking.merchantOperatingCityId booking.merchantId (show Spec.FRFS) (FRFSUtils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType)))
        >>= fromMaybeM (InternalError "Beckn Config not found")
    updateTotalOrderValueAndSettlementAmount booking quoteCategories bapConfig
    -- Must stay LAST and swallowed, or a Redis error skips the seat release and settlement above.
    -- Design notes: scripts/testing/cancel/DESIGN.md
    when fullyCancelled $ do
      quotaResult <- try @_ @SomeException $ do
        mbQuota <- FRFSUtils.getCancellationQuota booking
        whenJust mbQuota FRFSUtils.markCancellationCounted
      case quotaResult of
        Left err -> logError $ "FRFS cancellation quota not recorded for bookingId-" <> booking.id.getId <> ": " <> show err
        Right () -> pure ()
  return (mRiderNumber, person.mobileCountryCode, fareParameters)

-- | Side effects (SMS + Google Wallet) that require concrete Flow due to generic-lens constraints.
-- Call this after handleCancelledStatus from a Flow context.
handleCancelledSideEffects :: DFRFSTicketBooking.FRFSTicketBooking -> Maybe Text -> Maybe Text -> FRFSUtils.FRFSFareParameters -> Flow ()
handleCancelledSideEffects booking mRiderNumber mRiderMobileCountryCode fareParameters = do
  sendTicketCancelSMS mRiderNumber mRiderMobileCountryCode booking fareParameters
  handleGoogleWalletStatusUpdate booking

releaseSeatsIfHeld :: (MonadFlow m, Redis.HedisFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> [FRFSQuoteCategory.FRFSQuoteCategory] -> m ()
releaseSeatsIfHeld booking quoteCategories = do
  let allSeatIds = nub $ concatMap (fromMaybe [] . (.seatIds)) quoteCategories
  case (booking.tripId, booking.fromStopIdx, booking.toStopIdx) of
    (Just tripId, Just fromIdx, Just toIdx) | not (null allSeatIds) -> do
      logInfo $ "Releasing seats on cancel for bookingId: " <> booking.id.getId <> ", tripId: " <> tripId
      SeatBooking.releaseConfirmedSeats tripId allSeatIds fromIdx toIdx
    _ -> pure ()

checkRefundAndCancellationCharges :: (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> HighPrecMoney -> HighPrecMoney -> m ()
checkRefundAndCancellationCharges bookingId refundAmount cancellationCharges = do
  booking <- runInReplica $ QTBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case booking of
    DFRFSTicketBooking.FRFSTicketBooking {refundAmount = Just rfAmount, cancellationCharges = Just charges} -> do
      when (Just rfAmount /= Just refundAmount) $
        throwError $ InternalError $ "Refund Amount mismatch in onCancel Req " <> "refundAmount: " <> show refundAmount <> " rfAmount: " <> show rfAmount
      when (Just charges /= Just cancellationCharges) $
        throwError $ InternalError $ "Cancellation Charges mismatch in onCancel Req " <> "cancellationCharges: " <> show cancellationCharges <> " charges: " <> show charges
    _ -> throwError $ InternalError "Refund Amount or Cancellation Charges not found in booking"

sendTicketCancelSMS :: Maybe Text -> Maybe Text -> DFRFSTicketBooking.FRFSTicketBooking -> FRFSUtils.FRFSFareParameters -> Flow ()
sendTicketCancelSMS mRiderNumber mRiderMobileCountryCode booking fareParameters =
  whenJust booking.partnerOrgId $ \pOrgId -> do
    fork "send ticket cancel sms" $
      withLogTag ("SMS:FRFSBookingId:" <> booking.id.getId) $ do
        mobileNumber <- mRiderNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
        let mocId = booking.merchantOperatingCityId
            countryCode = fromMaybe "+91" mRiderMobileCountryCode
            phoneNumber = countryCode <> mobileNumber

        mbBuildSmsReq <-
          MessageBuilder.buildFRFSTicketCancelMessage mocId pOrgId $
            MessageBuilder.BuildFRFSTicketCancelMessageReq
              { countOfTickets = fareParameters.totalQuantity,
                bookingId = booking.id
              }
        maybe
          (logError $ "SMS not sent, SMS template not found for partnerOrgId:" <> pOrgId.getId)
          ( \buildSmsReq -> do
              let smsReq = buildSmsReq phoneNumber
              Sms.sendSMS booking.merchantId mocId smsReq >>= Sms.checkSmsResult
          )
          mbBuildSmsReq

handleGoogleWalletStatusUpdate :: DFRFSTicketBooking.FRFSTicketBooking -> Flow ()
handleGoogleWalletStatusUpdate booking =
  whenJust booking.partnerOrgId $ \pOrgId -> do
    walletPOCfg <- do
      pOrgCfg <- CQPOC.findByIdAndCfgType pOrgId DPOC.WALLET_CLASS_NAME >>= fromMaybeM (PartnerOrgConfigNotFound pOrgId.getId $ show DPOC.WALLET_CLASS_NAME)
      DPOC.getWalletClassNameConfig pOrgCfg.config
    let mbClassName = HashMap.lookup booking.merchantOperatingCityId.getId walletPOCfg.className
    whenJust mbClassName $ \_ -> do
      fork ("updating status of tickets in google wallet for bookingId: " <> booking.id.getId) $ do
        tickets <- QTicket.findAllByTicketBookingId booking.id
        let serviceName = DEMSC.WalletService GW.GoogleWallet
        let mId = booking.merchantId
        let mocId' = booking.merchantOperatingCityId
        serviceAccount <- GWSA.getserviceAccount mId mocId' serviceName
        forM_ tickets $ \ticket -> do
          let googleStatus = GWLink.mapToGoogleTicketStatus ticket.status
          let resourceId = serviceAccount.saIssuerId <> "." <> ticket.id.getId
          let obj = TC.TransitObjectPatch {TC.state = show googleStatus}
          resp <- GWSA.getObjectGoogleWallet serviceAccount resourceId
          case resp of
            Nothing -> return ()
            Just _ -> do
              void $ GWSA.updateTicketStatusForGoogleWallet obj serviceAccount resourceId
              return ()
