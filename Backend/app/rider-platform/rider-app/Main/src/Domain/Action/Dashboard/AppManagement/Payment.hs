module Domain.Action.Dashboard.AppManagement.Payment
  ( getPaymentRefundRequestList,
    getPaymentRefundRequestInfo,
    postPaymentRefundRequestRespond,
  )
where

import qualified API.Types.Dashboard.AppManagement.Payment as Common
import Control.Applicative ((<|>))
import qualified Dashboard.Common as Common
import qualified Domain.Action.UI.RidePayment as DRidePayment
import qualified Domain.Types.Merchant as DM
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
import qualified "payment" Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import qualified SharedLogic.Payment as SPayment
import qualified SharedLogic.PaymentInvoice as SPInvoice
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
  Id DPaymentOrder.PaymentOrder ->
  Maybe Bool ->
  Flow Common.RefundRequestInfoResp
getPaymentRefundRequestInfo merchantShortId opCity orderId refreshRefunds = do
  let refundRequestInfoHandler =
        DRidePayment.RefundRequestInfoHandler
          { validateRefundRequestOwner = \refundRequest -> do
              merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
              merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
              unless (refundRequest.merchantOperatingCityId == merchantOpCity.id) $ throwError (RefundRequestDoesNotExist orderId.getId),
            mkRefundRequestInfoResp = mkRefundRequestInfoResp,
            fetchRefunds = \refundsId -> (Just <$>) $ QRefunds.findById refundsId >>= fromMaybeM (InvalidRequest $ "No refunds matches passed data \"" <> refundsId.getId <> "\" not exist."), -- required only for admin
            notifyRefunds = Notify.notifyRefunds
          }
  let rideId = cast @DPaymentOrder.PaymentOrder @DRide.Ride orderId
  DRidePayment.fetchPaymentRefundRequestInfo @Common.RefundRequestInfoResp refundRequestInfoHandler refreshRefunds rideId

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
  Id DPaymentOrder.PaymentOrder ->
  Common.RefundRequestRespondReq ->
  Flow Common.RefundRequestRespondResp
postPaymentRefundRequestRespond merchantShortId opCity orderId req = do
  eResult <- Redis.whenWithLockRedisAndReturnValue (DRidePayment.refundRequestProccessingKey orderId) 60 $ do
    merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
    merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
    refundRequest <- QRefundRequest.findByOrderId orderId >>= fromMaybeM (RefundRequestDoesNotExist orderId.getId)
    unless (refundRequest.merchantOperatingCityId == merchantOpCity.id) $ throwError (RefundRequestDoesNotExist orderId.getId)
    whenJust req.approvedAmount $ \approvedAmount -> do
      unless (approvedAmount.currency == refundRequest.currency) $
        throwError (InvalidRequest "Invalid currency")
      if req.approve
        then when (approvedAmount.amount > refundRequest.transactionAmount) $ do
          throwError (InvalidRequest "Couldn't refund more than transaction amount")
        else throwError (InvalidRequest "Approved amount not required if refund request wasn't approved")

    case (refundRequest.status, req.approve) of
      (DRefundRequest.OPEN, True) -> initiateRefunds merchantOpCity refundRequest
      (DRefundRequest.OPEN, False) -> rejectRefunds refundRequest
      (DRefundRequest.FAILED, True) -> initiateRetryRefunds merchantOpCity refundRequest -- initiate new attempt
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
    initiateRetryRefunds merchantOpCity refundRequest = do
      unless (req.retryRefunds == Just True) $
        throwError (InvalidRequest "Refund was failed. Set retryRefund flag for new attempt")
      initiateRefunds merchantOpCity refundRequest

    initiateRefunds merchantOpCity refundRequest = do
      logInfo $ "Refund request approved by admin: orderId: " <> orderId.getId <> ". Initiate refunds: refundsTries: " <> show (refundRequest.refundsTries + 1)
      let updRefundsAmount = (req.approvedAmount <&> (.amount)) <|> refundRequest.requestedAmount <|> Just refundRequest.transactionAmount

      -- amount can be updated only for retry case, because new Stripe refund object will be created
      when (refundRequest.refundsTries > 0 && req.retryRefunds /= Just True) $
        whenJust refundRequest.refundsAmount $ \existingRefundsAmount -> do
          unless (updRefundsAmount == Just existingRefundsAmount) $
            throwError (InvalidRequest $ "Could not change refunds amount, as refund already was initiated: current refunds amount: " <> show existingRefundsAmount)

      QRefundRequest.updateRefundDetails DRefundRequest.APPROVED req.responseDescription updRefundsAmount (refundRequest.refundsTries + 1) refundRequest.id

      -- pass already updated refundRequest entry
      let updRefundRequest =
            refundRequest{status = DRefundRequest.APPROVED,
                          responseDescription = req.responseDescription,
                          refundsAmount = updRefundsAmount,
                          refundsTries = refundRequest.refundsTries + 1
                         }
      let rideId = cast @DPaymentOrder.PaymentOrder @DRide.Ride updRefundRequest.orderId
      QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
      Notify.notifyRefunds updRefundRequest
      initiateRefundsApiCall merchantOpCity updRefundRequest

    initiateRefundsApiCall merchantOpCity refundRequest = do
      let rideId = cast @DPaymentOrder.PaymentOrder @DRide.Ride refundRequest.orderId
      ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
      booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
      let initiateStripeRefundReq =
            DPayment.InitiateStripeRefundReq
              { orderId = refundRequest.orderId,
                transactionId = refundRequest.transactionId,
                amount = (req.approvedAmount <&> (.amount)) <|> refundRequest.requestedAmount, -- full transaction.amount would be refunded in case if not specified
                driverAccountId,
                email = Nothing, -- driver email is not mandatory, as driver bank account already created
                retryRefunds = fromMaybe False req.retryRefunds
              }
      SPayment.makeStripeRefund merchantOpCity.merchantId merchantOpCity.id booking.paymentMode initiateStripeRefundReq >>= \case
        Left err -> do
          logError $ "Failed to refund: orderId: " <> orderId.getId <> "; err: " <> err
          QRefundRequest.updateRefundIdAndStatus refundRequest.refundsId DRefundRequest.FAILED refundRequest.id
          let updRefundRequest = refundRequest{status = DRefundRequest.FAILED}
          QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
          Notify.notifyRefunds updRefundRequest
          pure Common.RefundRequestRespondResp {status = updRefundRequest.status, refundStatus = Nothing, errorCode = Nothing}
        Right result -> do
          let updStatus = DRidePayment.castRefundRequestStatus result.status
          when (refundRequest.refundsId /= Just result.refundsId || refundRequest.status /= updStatus) $ -- old status is APPROVED
            QRefundRequest.updateRefundIdAndStatus (Just result.refundsId) updStatus refundRequest.id
          when (refundRequest.status /= updStatus) $ do
            let updRefundRequest = refundRequest{refundsId = Just result.refundsId, status = updStatus}
            QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
            Notify.notifyRefunds updRefundRequest
          -- Create or update refund invoice
          let refundAmount = fromMaybe refundRequest.transactionAmount refundRequest.refundsAmount
              invoiceStatus = SPInvoice.refundStatusToInvoiceStatus result.status
              paymentPurpose = SPInvoice.refundPurposeToPaymentPurpose refundRequest.refundPurpose
          SPInvoice.createOrUpdateRefundInvoice merchantShortId rideId orderId paymentPurpose invoiceStatus refundAmount refundRequest.currency merchantOpCity.merchantId merchantOpCity.id
          pure Common.RefundRequestRespondResp {status = updStatus, refundStatus = Just result.status, errorCode = result.errorCode}

    rejectRefunds refundRequest = do
      logInfo $ "Refund request rejected by admin: orderId: " <> orderId.getId
      QRefundRequest.updateRefundDetails DRefundRequest.REJECTED req.responseDescription refundRequest.refundsAmount refundRequest.refundsTries refundRequest.id
      let updRefundRequest = refundRequest{status = DRefundRequest.REJECTED, responseDescription = req.responseDescription}
      let rideId = cast @DPaymentOrder.PaymentOrder @DRide.Ride updRefundRequest.orderId
      QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
      Notify.notifyRefunds updRefundRequest
      pure Common.RefundRequestRespondResp {status = updRefundRequest.status, refundStatus = Nothing, errorCode = Nothing}
