module Domain.Action.Dashboard.AppManagement.Payment
  ( getPaymentRefundRequestList,
    getPaymentRefundRequestInfo,
    postPaymentRefundRequestRespond,
    postPaymentRefundRequestInitiate,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment as Common
import Control.Applicative ((<|>))
import qualified Dashboard.Common as Common
import qualified Domain.Action.UI.RidePayment as DRidePayment
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.Person as DP
import qualified "this" Domain.Types.RefundRequest as DRefundRequest
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Payment.Interface
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified Lib.Payment.Storage.HistoryQueries.Refunds as HQRefunds
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

getPaymentRefundRequestList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe DRefundRequest.RefundRequestStatus ->
  Maybe DRefundRequest.RefundRequestCode ->
  Maybe (Id DP.Person) ->
  Maybe (Id DPaymentOrder.PaymentOrder) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.RefundRequestResp
getPaymentRefundRequestList merchantShortId opCity mbLimit mbOffset status code customerId orderId from to = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  refundRequests <- runInReplica $ QRefundRequest.findAllRefundRequestItem merchantOpCity.id limit offset status code customerId orderId from to
  let count = length refundRequests
  let summary = Common.Summary {totalCount = count, count}
  pure $ Common.RefundRequestResp {refundRequests = mkRefundRequestItem <$> refundRequests, summary = summary}
  where
    maxLimit = 20
    defaultLimit = 10

mkRefundRequestItem :: DRefundRequest.RefundRequest -> Common.RefundRequestItem
mkRefundRequestItem DRefundRequest.RefundRequest {..} =
  Common.RefundRequestItem
    { refundsAmount = flip PriceAPIEntity currency <$> refundsAmount,
      requestedAmount = flip PriceAPIEntity currency <$> requestedAmount,
      transactionAmount = PriceAPIEntity transactionAmount currency,
      ..
    }

getPaymentRefundRequestInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRefundRequest.RefundRequest ->
  Maybe Bool ->
  Flow Common.RefundRequestInfoResp
getPaymentRefundRequestInfo merchantShortId opCity refundRequestId refreshRefunds = do
  refundRequest <- QRefundRequest.findById refundRequestId >>= fromMaybeM (RefundRequestDoesNotExist refundRequestId.getId)
  let refundRequestInfoHandler =
        DRidePayment.RefundRequestInfoHandler
          { validateRefundRequestOwner = \rr -> do
              merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
              merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
              unless (rr.merchantOperatingCityId == merchantOpCity.id) $ throwError (RefundRequestDoesNotExist refundRequestId.getId),
            mkRefundRequestInfoResp = mkRefundRequestInfoResp,
            fetchRefunds = \refundsId -> (Just <$>) $ HQRefunds.findById refundsId >>= fromMaybeM (InvalidRequest $ "No refunds matches passed data \"" <> refundsId.getId <> "\" not exist.") -- required only for admin
          }
  DRidePayment.fetchPaymentRefundRequestInfo @Common.RefundRequestInfoResp refundRequestInfoHandler refreshRefunds refundRequest

mkRefundRequestInfoResp ::
  DRefundRequest.RefundRequest ->
  Maybe Text ->
  Maybe Kernel.External.Payment.Interface.RefundStatus ->
  Maybe Text ->
  Common.RefundRequestInfoResp
mkRefundRequestInfoResp DRefundRequest.RefundRequest {..} evidence refundStatus errorCode =
  Common.RefundRequestInfoResp
    { refundsAmount = flip PriceAPIEntity currency <$> refundsAmount,
      requestedAmount = flip PriceAPIEntity currency <$> requestedAmount,
      transactionAmount = PriceAPIEntity transactionAmount currency,
      ..
    }

postPaymentRefundRequestRespond ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRefundRequest.RefundRequest ->
  Common.RefundRequestRespondReq ->
  Flow Common.RefundRequestRespondResp
postPaymentRefundRequestRespond merchantShortId opCity refundRequestId req = do
  -- Resolve the row before acquiring the lock so the per-orderId key can be derived from it.
  refundRequest <- QRefundRequest.findById refundRequestId >>= fromMaybeM (RefundRequestDoesNotExist refundRequestId.getId)
  let orderId = refundRequest.orderId
  eResult <- Redis.whenWithLockRedisAndReturnValue (DRidePayment.refundRequestProccessingKey orderId) 60 $ do
    merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
    merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
    unless (refundRequest.merchantOperatingCityId == merchantOpCity.id) $ throwError (RefundRequestDoesNotExist refundRequestId.getId)
    -- Currency-only check here; the amount bound is enforced cumulatively inside initiateRefunds.
    whenJust req.approvedAmount $ \approvedAmount -> do
      unless (approvedAmount.currency == refundRequest.currency) $
        throwError (InvalidRequest "Invalid currency")
      unless req.approve $
        throwError (InvalidRequest "Approved amount not required if refund request wasn't approved")

    case (refundRequest.status, req.approve) of
      (DRefundRequest.OPEN, True) -> initiateRefunds refundRequest
      (DRefundRequest.OPEN, False) -> rejectRefunds refundRequest
      (DRefundRequest.FAILED, True) -> initiateRetryRefunds refundRequest -- initiate new attempt
      (DRefundRequest.FAILED, False) -> rejectRefunds refundRequest
      (DRefundRequest.APPROVED, True) -> throwError (InvalidRequest "Refund request already approved")
      (DRefundRequest.APPROVED, False) -> throwError (InvalidRequest "Couldn't reject refund request as it's already approved and refunds initiated")
      (DRefundRequest.REJECTED, True) -> throwError (InvalidRequest "Refund request already rejected") -- should we allow approve if previously rejected?
      (DRefundRequest.REJECTED, False) -> throwError (InvalidRequest "Refund request already rejected")
      (DRefundRequest.REFUNDED, _) -> throwError (InvalidRequest "Refunds already processed")
  case eResult of
    Left _ -> do
      logError $ "Order refund already in progress: " <> orderId.getId
      throwError (InvalidRequest "Order refund already in progress")
    Right result -> return result
  where
    initiateRetryRefunds refundRequest = do
      unless (req.retryRefunds == Just True) $
        throwError (InvalidRequest "Refund was failed. Set retryRefund flag for new attempt")
      initiateRefunds refundRequest

    initiateRefunds refundRequest = do
      logInfo $ "Refund request approved by admin: orderId: " <> refundRequest.orderId.getId <> ". Initiate refunds: refundsTries: " <> show (refundRequest.refundsTries + 1)
      let updRefundsAmount = (req.approvedAmount <&> (.amount)) <|> refundRequest.requestedAmount <|> Just refundRequest.transactionAmount

      -- amount can be updated only for retry case, because new Stripe refund object will be created
      when (refundRequest.refundsTries > 0 && req.retryRefunds /= Just True) $
        whenJust refundRequest.refundsAmount $ \existingRefundsAmount -> do
          unless (updRefundsAmount == Just existingRefundsAmount) $
            throwError (InvalidRequest $ "Could not change refunds amount, as refund already was initiated: current refunds amount: " <> show existingRefundsAmount)

      -- Fetch siblings once for both checks below (single-in-flight + cumulative).
      existingRequests <- QRefundRequest.findAllByOrderId refundRequest.orderId
      when (any (\r -> r.status == DRefundRequest.APPROVED && r.id /= refundRequest.id) existingRequests) $
        throwError (InvalidRequest $ "Another refund request is in flight for order: " <> refundRequest.orderId.getId)
      whenJust updRefundsAmount $ \amt -> do
        transaction <-
          HQPaymentTransaction.findEarliestChargedTransactionByOrderId refundRequest.orderId
            >>= fromMaybeM (InvalidRequest "No transaction found for refund")
        DRidePayment.validateRefundAmount existingRequests (Just (PriceAPIEntity amt refundRequest.currency)) transaction

      QRefundRequest.updateRefundDetails DRefundRequest.APPROVED req.responseDescription updRefundsAmount (refundRequest.refundsTries + 1) req.deductFromDriver refundRequest.id

      -- pass already updated refundRequest entry
      let updRefundRequest =
            refundRequest{status = DRefundRequest.APPROVED,
                          responseDescription = req.responseDescription,
                          refundsAmount = updRefundsAmount,
                          refundsTries = refundRequest.refundsTries + 1,
                          deductFromDriver = req.deductFromDriver
                         }
      rideId <- SPayment.getRideIdForOrder updRefundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> updRefundRequest.orderId.getId)
      QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
      Notify.notifyRefunds updRefundRequest
      DRidePayment.processRefundRaised updRefundRequest
      submitRefundToPaymentService updRefundRequest (fromMaybe False req.retryRefunds)

    rejectRefunds refundRequest = do
      logInfo $ "Refund request rejected by admin: orderId: " <> refundRequest.orderId.getId
      QRefundRequest.updateRefundDetails DRefundRequest.REJECTED req.responseDescription refundRequest.refundsAmount refundRequest.refundsTries Nothing refundRequest.id
      let updRefundRequest = refundRequest{status = DRefundRequest.REJECTED, responseDescription = req.responseDescription}
      rideId <- SPayment.getRideIdForOrder updRefundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> updRefundRequest.orderId.getId)
      QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
      Notify.notifyRefunds updRefundRequest
      pure Common.RefundRequestRespondResp {status = updRefundRequest.status, refundStatus = Nothing, errorCode = Nothing}

submitRefundToPaymentService ::
  DRefundRequest.RefundRequest ->
  Bool ->
  Flow Common.RefundRequestRespondResp
submitRefundToPaymentService refundRequest retryIfFailed = do
  rideId <- SPayment.getRideIdForOrder refundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> refundRequest.orderId.getId)
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
  let commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity refundRequest.merchantOperatingCityId
  let refundReq =
        DPayment.RefundPaymentServiceReq
          { orderId = refundRequest.orderId,
            merchantOpCityId = commonMerchantOperatingCityId,
            driverAccountId = Just driverAccountId,
            email = Nothing,
            amount = refundRequest.refundsAmount,
            deductFromDriver = refundRequest.deductFromDriver,
            retryIfFailed = retryIfFailed,
            -- targets THIS refund, so the payment lib's latest-by-order lookup doesn't
            -- no-op the attempt when a prior sibling on the order already succeeded.
            refundsId = refundRequest.refundsId
          }
  SPayment.makeRefundPayment refundRequest.merchantId refundRequest.merchantOperatingCityId booking.paymentMode refundReq >>= \case
    Nothing -> do
      logError $ "Failed to refund: orderId: " <> refundRequest.orderId.getId
      DRidePayment.processRefundResult refundRequest DRefundRequest.FAILED refundRequest.refundsId
      pure Common.RefundRequestRespondResp {status = DRefundRequest.FAILED, refundStatus = Nothing, errorCode = Nothing}
    Just result -> do
      let updStatus = DRidePayment.castRefundRequestStatus result.status
          refundId = Id result.refundId
      DRidePayment.processRefundResult refundRequest updStatus (Just refundId)
      pure Common.RefundRequestRespondResp {status = updStatus, refundStatus = Just result.status, errorCode = result.errorCode}

postPaymentRefundRequestInitiate ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DRide.Ride ->
  Common.RefundRequestInitiateReq ->
  Flow Common.RefundRequestRespondResp
postPaymentRefundRequestInitiate merchantShortId opCity rideId req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let h =
        DRidePayment.RefundRequestCreateHandler
          { validateRefundRequester = \booking ->
              unless (booking.merchantOperatingCityId == merchantOpCity.id) $ throwError (RideNotFound rideId.getId),
            mkRefundRequestRow = \ctx -> do
              DRidePayment.validateRefundAmount ctx.existingRequests req.requestedAmount ctx.transaction
              let requestedAmount = req.requestedAmount <&> (.amount)
                  refundsAmount = requestedAmount <|> Just ctx.transaction.amount
                  refundsTries = 1 -- row is born post-approval, matching /respond's increment
                  evidenceS3Path = Nothing -- no need as initiated by admin
                  code = DRefundRequest.RefundRequestCode "ADMIN_INITIATED"
                  description = fromMaybe "Admin-initiated refund" req.description
              DRidePayment.buildRefundRequestRow
                ctx
                evidenceS3Path
                code
                description
                requestedAmount
                refundsAmount
                refundsTries
                DRefundRequest.APPROVED
                req.deductFromDriver,
            postCreate = \refundRequest -> do
              Notify.notifyRefunds refundRequest
              DRidePayment.processRefundRaised refundRequest
              submitRefundToPaymentService refundRequest False
          }
  DRidePayment.createPaymentRefundRequest h rideId
