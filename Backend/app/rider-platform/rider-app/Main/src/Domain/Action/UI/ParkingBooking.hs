module Domain.Action.UI.ParkingBooking (postMultimodalParkingBook, parkingBookingOrderStatusHandler) where

import qualified API.Types.UI.ParkingBooking
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.ParkingTransaction as DPT
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.ParkingTransaction as QPT
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Payment as TPayment

postMultimodalParkingBook ::
  ( Kernel.Prelude.Maybe Data.Text.Text ->
    API.Types.UI.ParkingBooking.ParkingBookingReq ->
    Environment.Flow API.Types.UI.ParkingBooking.ParkingBookingResponse
  )
postMultimodalParkingBook mbApiKey req = do
  -- Verify API key
  apiKey <- mbApiKey & fromMaybeM (MissingHeader "api-key")
  expectedApiKey <- asks (.parkingApiKey)
  unless (apiKey == expectedApiKey) $ throwError (InvalidRequest "Invalid API key")

  -- Get default merchant operating city
  person <- QPerson.findById req.customerId >>= fromMaybeM (PersonNotFound "Person not found")
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity person.merchantId
  let merchantOpCityId = merchantOpCity.id

  parkingTransactionId <- generateGUID
  paymentOrderId <- generateGUID
  orderShortId <- generateShortId
  now <- getCurrentTime

  let parkingTransaction =
        DPT.ParkingTransaction
          { id = Kernel.Types.Id.Id parkingTransactionId,
            amount = req.amount,
            startTime = req.parkingStartTime,
            endTime = req.parkingEndTime,
            parkingLotId = req.entityId,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            status = DPT.Pending,
            vehicleNumber = req.vehicleNumber,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QPT.create parkingTransaction

  isSplitEnabled <- TPayment.getIsSplitEnabled person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking
  isPercentageSplitEnabled <- TPayment.getIsPercentageSplit person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking
  splitSettlementDetails <- TPayment.mkSplitSettlementDetails isSplitEnabled req.amount [] isPercentageSplitEnabled

  customerEmail <- fromMaybe "noreply@nammayatri.in" <$> mapM decrypt person.email
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = paymentOrderId,
            orderShortId = orderShortId.getShortId,
            amount = req.amount,
            customerId = req.customerId.getId,
            customerEmail,
            customerPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing,
            splitSettlementDetails = splitSettlementDetails,
            basket = Nothing
          }

  let commonMerchantId = Kernel.Types.Id.cast @Domain.Types.Merchant.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person req.customerId
      createOrderCall = TPayment.createOrder person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking (Just req.customerId.getId) person.clientSdkVersion

  orderResp <- DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast merchantOpCityId) commonPersonId Nothing Nothing TPayment.ParkingBooking createOrderReq createOrderCall >>= fromMaybeM (InternalError "Failed to create payment order")

  paymentLink <- case orderResp.payment_links of
    Just links -> fromMaybeM (InternalError "Payment link not found") links.web
    Nothing -> throwError (InternalError "Payment link not found in response")

  return $
    API.Types.UI.ParkingBooking.ParkingBookingResponse
      { parkingLotId = parkingTransactionId,
        orderShortId = orderShortId.getShortId,
        paymentLink = Kernel.Prelude.showBaseUrl paymentLink,
        orderId = Kernel.Types.Id.Id paymentOrderId
      }

-- Webhook Handler for Pass Payment Status Updates
parkingBookingOrderStatusHandler ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Kernel.Types.Id.Id DOrder.PaymentOrder ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Payment.TransactionStatus ->
  m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
parkingBookingOrderStatusHandler paymentOrderId _merchantId status = do
  logInfo $ "Parking booking payment webhook handler called for paymentOrderId: " <> paymentOrderId.getId
  mbParkingTransaction <- QPT.findByPaymentOrderId paymentOrderId
  case mbParkingTransaction of
    Just parkingTransaction -> do
      let mbParkingStatus = convertPaymentStatusToParkingTransactionStatus status
      whenJust mbParkingStatus $ \parkingStatus -> QPT.updateStatusById parkingStatus parkingTransaction.id
      case mbParkingStatus of
        Just DPT.Booked -> return (DPayment.FulfillmentSucceeded, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.Failed -> return (DPayment.FulfillmentFailed, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.Refunded -> return (DPayment.FulfillmentRefunded, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.RefundInitiated -> return (DPayment.FulfillmentRefundInitiated, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.RefundPending -> return (DPayment.FulfillmentRefundPending, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.RefundFailed -> return (DPayment.FulfillmentRefundFailed, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        _ -> return (DPayment.FulfillmentPending, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
    _ -> do
      logError $ "Parking transaction not found for paymentOrderId: " <> paymentOrderId.getId
      return (DPayment.FulfillmentPending, Nothing, Nothing)
  where
    convertPaymentStatusToParkingTransactionStatus = \case
      Payment.CHARGED -> Just DPT.Booked
      Payment.AUTHENTICATION_FAILED -> Just DPT.Failed
      Payment.AUTHORIZATION_FAILED -> Just DPT.Failed
      Payment.JUSPAY_DECLINED -> Just DPT.Failed
      Payment.CANCELLED -> Just DPT.Failed
      Payment.AUTO_REFUNDED -> Just DPT.Refunded
      _ -> Nothing
