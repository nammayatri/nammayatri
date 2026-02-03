{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Payment
  ( DPayment.PaymentStatusResp (..),
    createOrder,
    createRideBookingPaymentOrder,
    getRideBookingPaymentStatusByBookingId,
    getStatus,
    getStatusS2S,
    getOrder,
    juspayWebhookHandler,
    paytmEdcCallbackHandler,
    PaytmEdcCallbackReq (..),
    rideBookingOrderStatusHandler,
    stripeWebhookHandler,
    postWalletRecharge,
    getWalletBalance,
    stripeTestWebhookHandler,
  )
where

import qualified API.Types.UI.Payment as PaymentAPI
import qualified Beckn.ACL.Confirm as ACL
import Control.Applicative ((<|>))
import qualified Data.Text
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.BBPS as BBPS
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Action.UI.ParkingBooking as ParkingBooking
import qualified Domain.Action.UI.Pass as Pass
import qualified Domain.Action.UI.RidePayment as DRidePayment
import qualified Domain.Types.Booking as DRB
import Domain.Types.Extra.MerchantPaymentMethod ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface.Events.Types as PEInterface
import qualified Kernel.External.Payment.Interface as KPayment
import qualified Kernel.External.Payment.Interface.Juspay as Juspay
import qualified Kernel.External.Payment.Interface.Stripe as Stripe
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Payment.Stripe.Webhook (RawByteString (..))
import qualified Kernel.External.Payment.Types as Payment
import qualified Kernel.External.Wallet as Wallet
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (Value)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PersonWallet as DPersonWallet
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PersonWallet as QPersonWallet
import Servant (BasicAuthData)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Payment as SPayment
import qualified SharedLogic.PaymentInvoice as SPInvoice
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.PlaceBasedServiceConfig as CQPBSC
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.PaymentInvoiceExtra as QPaymentInvoiceExtra
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.TicketBooking as QTB
import Data.Aeson (defaultOptions, withObject, (.:?))
import Data.Aeson.Types ()
import Data.OpenApi ()
import GHC.Generics ()
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify
import qualified Tools.Payment as Payment
import qualified Tools.Wallet as TWallet

-- create order -----------------------------------------------------

createOrder ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DRide.Ride ->
  Flow Payment.CreateOrderResp
createOrder (personId, merchantId) rideId = do
  logInfo $ "Order created for rideId" <> show rideId
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  -- ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus $ show ride.status)
  totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  -- person <- QP.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  riderId <- B.runInReplica $ QRide.findRiderIdByRideId ride.id >>= fromMaybeM (InternalError "riderId not found")
  -- riderId <- QRide.findRiderIdByRideId ride.id >>= fromMaybeM (InternalError "riderId not found")
  unless (person.id == riderId) $ throwError NotAnExecutor
  customerEmail <- person.email & fromMaybeM (PersonFieldNotPresent "email") >>= decrypt
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing Payment.Normal
  isPercentageSplitEnabled <- Payment.getIsPercentageSplit merchantId person.merchantOperatingCityId Nothing Payment.Normal
  splitSettlementDetails <- Payment.mkSplitSettlementDetails isSplitEnabled totalFare.amount [] isPercentageSplitEnabled False
  staticCustomerId <- SLUtils.getStaticCustomerId person customerPhone
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = rideId.getId,
            orderShortId = ride.shortId.getShortId, -- should be Alphanumeric with character length less than 18.
            amount = totalFare.amount,
            customerId = staticCustomerId,
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
            metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
            splitSettlementDetails = splitSettlementDetails,
            basket = Nothing
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let createOrderCall = Payment.createOrder merchantId person.merchantOperatingCityId Nothing Payment.Normal (Just person.id.getId) person.clientSdkVersion (Just False)
      createWalletCall = TWallet.createWallet merchantId person.merchantOperatingCityId
  DPayment.createOrderService commonMerchantId (Just $ cast person.merchantOperatingCityId) commonPersonId Nothing Nothing Payment.Normal isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) False Nothing >>= fromMaybeM (InternalError "Order expired please try again")

-- | Create a RideBooking payment order (payment-before-confirm flow). Sets domainEntityId = bookingId on the order.
createRideBookingPaymentOrder :: DRB.Booking -> Flow (Maybe Payment.CreateOrderResp)
createRideBookingPaymentOrder booking = do
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  let merchantOperatingCityId = booking.merchantOperatingCityId
  customerEmail <- fromMaybe "noreply@nammayatri.in" <$> mapM decrypt person.email
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  staticCustomerId <- SLUtils.getStaticCustomerId person customerPhone
  paymentOrderId <- generateGUID
  orderShortId <- generateShortId
  let amount = booking.estimatedTotalFare.amount
  isSplitEnabled <- Payment.getIsSplitEnabled booking.merchantId merchantOperatingCityId Nothing DOrder.RideBooking
  isPercentageSplitEnabled <- Payment.getIsPercentageSplit booking.merchantId merchantOperatingCityId Nothing DOrder.RideBooking
  splitSettlementDetails <- Payment.mkSplitSettlementDetails isSplitEnabled amount [] isPercentageSplitEnabled False
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = paymentOrderId,
            orderShortId = orderShortId.getShortId,
            amount,
            customerId = staticCustomerId,
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
  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant booking.merchantId
      commonPersonId = cast @DP.Person @DPayment.Person person.id
      createOrderCall = Payment.createOrder booking.merchantId merchantOperatingCityId Nothing DOrder.RideBooking (Just person.id.getId) person.clientSdkVersion Nothing
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let createWalletCall = TWallet.createWallet booking.merchantId merchantOperatingCityId
  mbOrderResp <- DPayment.createOrderService commonMerchantId (Just $ cast merchantOperatingCityId) commonPersonId Nothing Nothing DOrder.RideBooking isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) False
  whenJust mbOrderResp $ \resp -> do
    void $ QOrder.updatePaymentFulfillmentStatus (Id paymentOrderId) (Just DPayment.FulfillmentPending) (Just booking.id.getId) Nothing
    whenJust resp.payment_links $ \links ->
      whenJust links.web $ \url ->
        void $ QRideB.updatePaymentInfo booking.id booking.estimatedFare booking.discount booking.estimatedTotalFare (Just $ showBaseUrl url)
  pure mbOrderResp

-- | Get payment status by booking id. With Paytm EDC callback, confirm is triggered server-side on success; this is optional (UI refresh / fallback only).
getRideBookingPaymentStatusByBookingId ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DRB.Booking ->
  Flow DPayment.PaymentStatusResp
getRideBookingPaymentStatusByBookingId (personId, merchantId) bookingId = do
  booking <- QRideB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError NotAnExecutor
  paymentOrder <- QOrder.findByDomainEntityId bookingId.getId >>= fromMaybeM (PaymentOrderNotFound bookingId.getId)
  getStatus (personId, merchantId) paymentOrder.id

-- order status -----------------------------------------------------

getStatus ::
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  Flow DPayment.PaymentStatusResp
getStatus (personId, merchantId) orderId = do
  paymentOrder <- QOrder.findById orderId |<|>| QOrder.findByShortId (ShortId orderId.getId) >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  let paymentServiceType = fromMaybe DOrder.Normal paymentOrder.paymentServiceType
      fulfillmentHandler = mkFulfillmentHandler paymentServiceType (cast paymentOrder.merchantId) paymentOrder.id
  currentOrderStatus <- SPayment.syncOrderStatus fulfillmentHandler merchantId personId paymentOrder
  -- Check if current order is not successful and has a groupId
  case (currentOrderStatus.paymentFulfillmentStatus, paymentOrder.groupId) of
    (Just DPayment.FulfillmentSucceeded, _) -> pure currentOrderStatus
    (_, Nothing) -> pure currentOrderStatus
    (_, Just groupId) -> do
      now <- getCurrentTime
      otherOrders <- QOrder.findAllByGroupId groupId now
      let otherOrdersInGroup = filter (\o -> o.id /= paymentOrder.id) otherOrders
          successOtherOrder = find (\o -> o.paymentFulfillmentStatus == Just DPayment.FulfillmentSucceeded) otherOrdersInGroup
      fork "syncRemainingOrdersInGroup" $ do
        let remainingOrders = filter (\o -> maybe True (\successOrder -> o.id /= successOrder.id) successOtherOrder) otherOrdersInGroup
        mapM_
          ( \order -> do
              let orderPaymentServiceType = fromMaybe DOrder.Normal order.paymentServiceType
                  orderFulfillmentHandler = mkFulfillmentHandler orderPaymentServiceType (cast order.merchantId) order.id
              void $ SPayment.syncOrderStatus orderFulfillmentHandler merchantId personId order
          )
          remainingOrders
      -- Check if any other order has FulfillmentSucceeded status
      case successOtherOrder of
        Just successfulOrder -> do
          let successPaymentServiceType = fromMaybe DOrder.Normal successfulOrder.paymentServiceType
              successFulfillmentHandler = mkFulfillmentHandler successPaymentServiceType (cast successfulOrder.merchantId) successfulOrder.id
          logInfo $ "Found successful order in group: " <> successfulOrder.id.getId <> ", syncing instead of current order: " <> paymentOrder.id.getId
          SPayment.syncOrderStatus successFulfillmentHandler merchantId personId successfulOrder
        Nothing -> pure currentOrderStatus

-- order status s2s -----------------------------------------------------
getStatusS2S :: Id DOrder.PaymentOrder -> Id DP.Person -> Id DM.Merchant -> Maybe Data.Text.Text -> Flow DPayment.PaymentStatusResp
getStatusS2S orderId personId merchantId mbApiKey = do
  -- Verify API key
  apiKey <- mbApiKey & fromMaybeM (MissingHeader "api-key")
  expectedApiKey <- asks (.parkingApiKey)
  unless (apiKey == expectedApiKey) $ throwError (InvalidRequest "Invalid API key")
  getStatus (personId, merchantId) orderId

getOrder ::
  ( CacheFlow m r,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasKafkaProducer r
  ) =>
  (Id DP.Person, Id DM.Merchant) ->
  Id DOrder.PaymentOrder ->
  m DOrder.PaymentOrderAPIEntity
getOrder (personId, _) orderId = do
  order <- B.runInReplica $ QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  -- order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  unless (order.personId == cast personId) $ throwError NotAnExecutor
  mkOrderAPIEntity order

mkOrderAPIEntity :: (EncFlow m r, HasKafkaProducer r) => DOrder.PaymentOrder -> m DOrder.PaymentOrderAPIEntity
mkOrderAPIEntity DOrder.PaymentOrder {..} = do
  clientAuthToken_ <- decrypt `mapM` clientAuthToken
  return $ DOrder.PaymentOrderAPIEntity {clientAuthToken = clientAuthToken_, ..}

-- Paytm EDC callback types (flat ORDERID/STATUS or nested body.resultInfo)
data PaytmEdcCallbackReq = PaytmEdcCallbackReq
  { orderId :: Text,
    status :: Text,
    txnId :: Maybe Text,
    resultCode :: Maybe Text,
    resultMsg :: Maybe Text,
    checksumHash :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON PaytmEdcCallbackReq where
  parseJSON = withObject "PaytmEdcCallbackReq" $ \o -> do
    -- Support flat (ORDERID, STATUS) and nested (body.orderId, body.resultInfo.resultStatus)
    orderId' <-
      (o .:? "ORDERID" <|> o .:? "orderId")
        <|> (o .:? "body" >>= \mbBody -> case mbBody of
              Just bodyVal -> withObject "body" (\b -> b .:? "orderId") bodyVal
              Nothing -> pure Nothing)
        >>= \m -> case m of
          Nothing -> fail "Paytm callback: ORDERID/orderId required"
          Just t -> pure t
    status' <-
      (o .:? "STATUS" <|> o .:? "status")
        <|> (o .:? "body" >>= \mbBody -> case mbBody of
              Just bodyVal ->
                withObject "body" (\b -> b .:? "resultInfo" >>= \mbR -> case mbR of
                  Just rVal -> withObject "resultInfo" (\r -> r .:? "resultStatus") rVal
                  Nothing -> pure Nothing)
                  bodyVal
              Nothing -> pure Nothing)
        >>= \m -> case m of
          Nothing -> fail "Paytm callback: STATUS/resultStatus required"
          Just t -> pure t
    txnId' <- o .:? "TXNID" <|> o .:? "txnId"
      <|> (o .:? "body" >>= \mbBody -> case mbBody of
            Just bodyVal -> withObject "body" (\b -> b .:? "txnId") bodyVal
            Nothing -> pure Nothing)
    resultCode' <- o .:? "RESPCODE" <|> o .:? "resultCode"
      <|> (o .:? "body" >>= \mbBody -> case mbBody of
            Just bodyVal ->
              withObject "body" (\b -> b .:? "resultInfo" >>= \mbR -> case mbR of
                Just rVal -> withObject "resultInfo" (\r -> r .:? "resultCode") rVal
                Nothing -> pure Nothing)
                bodyVal
            Nothing -> pure Nothing)
    resultMsg' <- o .:? "RESPMSG" <|> o .:? "resultMsg"
      <|> (o .:? "body" >>= \mbBody -> case mbBody of
            Just bodyVal ->
              withObject "body" (\b -> b .:? "resultInfo" >>= \mbR -> case mbR of
                Just rVal -> withObject "resultInfo" (\r -> r .:? "resultMsg") rVal
                Nothing -> pure Nothing)
                bodyVal
            Nothing -> pure Nothing)
    checksumHash' <- o .:? "CHECKSUMHASH" <|> o .:? "checksumHash"
      <|> (o .:? "head" >>= \mbHead -> case mbHead of
            Just headVal -> withObject "head" (\h -> h .:? "signature") headVal
            Nothing -> pure Nothing)
    pure $
      PaytmEdcCallbackReq
        { orderId = orderId',
          status = status',
          txnId = txnId',
          resultCode = resultCode',
          resultMsg = resultMsg',
          checksumHash = checksumHash'
        }

instance ToJSON PaytmEdcCallbackReq where
  toJSON = genericToJSON defaultOptions

instance ToSchema PaytmEdcCallbackReq

-- Map Paytm resultStatus/STATUS to our TransactionStatus (TXN_SUCCESS/SUCCESS/A/S -> CHARGED, TXN_FAILURE/FAIL/F -> CANCELLED, else STARTED)
paytmStatusToTransactionStatus :: Text -> KPayment.TransactionStatus
paytmStatusToTransactionStatus s =
  case Data.Text.toUpper s of
    "TXN_SUCCESS" -> KPayment.CHARGED
    "SUCCESS" -> KPayment.CHARGED
    "A" -> KPayment.CHARGED
    "S" -> KPayment.CHARGED
    "TXN_FAILURE" -> KPayment.CANCELLED
    "FAIL" -> KPayment.CANCELLED
    "F" -> KPayment.CANCELLED
    "PENDING" -> KPayment.STARTED
    "U" -> KPayment.STARTED
    _ -> KPayment.STARTED

-- webhook ----------------------------------------------------------

fetchPaymentServiceConfig ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe Payment.PaymentServiceType ->
  Maybe Text ->
  Payment.PaymentService ->
  Flow Payment.PaymentServiceConfig
fetchPaymentServiceConfig merchantShortId mbCity mbServiceType mbPlaceId service = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let city = fromMaybe merchant.defaultCity mbCity
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
  let merchantId = merchant.id
  placeBasedConfig <- case mbPlaceId of
    Just id -> CQPBSC.findByPlaceIdAndServiceName (Id id) (DMSC.PaymentService service)
    Nothing -> return Nothing
  merchantServiceConfig' <- do
    CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCity.id (getPaymentServiceByType mbServiceType)
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOperatingCity.id.getId "Payment" (show service))
  case (placeBasedConfig <&> (.serviceConfig)) <|> Just merchantServiceConfig'.serviceConfig of
    Just (DMSC.PaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.MetroPaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.BusPaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.BbpsPaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.MultiModalPaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.PassPaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.ParkingPaymentServiceConfig vsc) -> pure vsc
    Just (DMSC.JuspayWalletServiceConfig vsc) -> pure vsc
    _ -> throwError $ InternalError "Unknown Service Config"
  where
    getPaymentServiceByType = \case
      Just Payment.Normal -> DMSC.PaymentService service
      Just Payment.BBPS -> DMSC.BbpsPaymentService service
      Just Payment.FRFSBooking -> DMSC.MetroPaymentService service
      Just Payment.FRFSBusBooking -> DMSC.BusPaymentService service
      Just Payment.FRFSMultiModalBooking -> DMSC.MultiModalPaymentService service
      Just Payment.FRFSPassPurchase -> DMSC.PassPaymentService service
      Just Payment.ParkingBooking -> DMSC.ParkingPaymentService service
      Just DOrder.Wallet -> DMSC.JuspayWalletService service
      Just DOrder.RideBooking -> DMSC.PaymentService service
      _ -> DMSC.PaymentService service

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe Payment.PaymentServiceType ->
  Maybe Text ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayWebhookHandler merchantShortId mbCity mbServiceType mbPlaceId authData value = do
  paymentServiceConfig <- fetchPaymentServiceConfig merchantShortId mbCity mbServiceType mbPlaceId Payment.Juspay
  orderWebhookResponse <- Juspay.orderStatusWebhook paymentServiceConfig DPayment.juspayWebhookService authData value
  osr <- case orderWebhookResponse of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  (orderShortId, status) <- getOrderData osr
  logDebug $ "order short Id from Response bap webhook: " <> show orderShortId
  whenJust mbServiceType $ \paymentServiceType -> do
    Redis.whenWithLockRedis (mkOrderStatusCheckKey orderShortId status) 60 $ do
      paymentOrder <- QOrder.findByShortId (ShortId orderShortId) >>= fromMaybeM (PaymentOrderNotFound orderShortId)
      mocId <- paymentOrder.merchantOperatingCityId & fromMaybeM (InternalError "MerchantOperatingCityId not found in payment order")
      person <- QP.findById (cast paymentOrder.personId) >>= fromMaybeM (InvalidRequest "Person not found")
      ticketPlaceId <-
        case paymentServiceType of
          Payment.Normal -> do
            ticketBooking <- QTB.findById (cast paymentOrder.id)
            return $ ticketBooking <&> (.ticketPlaceId)
          _ -> return Nothing
      let orderStatusCall = Payment.orderStatus (cast paymentOrder.merchantId) (cast mocId) ticketPlaceId paymentServiceType (Just paymentOrder.personId.getId) person.clientSdkVersion paymentOrder.isMockPayment
      void $ callWebhookHandlerWithOrderStatus paymentServiceType (ShortId orderShortId) orderStatusCall
  pure Ack
  where
    getOrderData osr = case osr of
      Payment.OrderStatusResp {..} -> pure (orderShortId, transactionStatus)
      _ -> throwError $ InternalError "Order Id not found in response."
    callWebhookHandlerWithOrderStatus paymentServiceType' orderShortId orderStatusCall = do
      paymentOrder <- QOrder.findByShortId orderShortId >>= fromMaybeM (PaymentOrderNotFound orderShortId.getShortId)
      let paymentServiceType = fromMaybe paymentServiceType' paymentOrder.paymentServiceType
          fulfillmentHandler = mkFulfillmentHandler paymentServiceType (cast paymentOrder.merchantId) paymentOrder.id
      SPayment.orderStatusHandler fulfillmentHandler paymentServiceType paymentOrder orderStatusCall

-- | Idempotent: confirm ride booking after payment success. Single place for Paytm EDC callback and rideBookingOrderStatusHandler.
confirmRideBookingFromPaymentOrder :: DOrder.PaymentOrder -> Flow (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
confirmRideBookingFromPaymentOrder paymentOrder = do
  bookingIdText <- paymentOrder.domainEntityId & fromMaybeM (InvalidRequest "RideBooking order has no domainEntityId")
  let bookingId = Id bookingIdText :: Id DRB.Booking
      confirmLockKey = "RideBooking:Confirm:" <> bookingIdText
      confirmTriggeredKey = "RideBooking:ConfirmTriggered:" <> bookingIdText
  Redis.whenWithLockRedis confirmLockKey 60 $ do
    alreadyTriggered <- Redis.get @Text confirmTriggeredKey
    unless (alreadyTriggered == Just "1") $ do
      onInitRes <- DOnInit.buildOnInitResFromBooking bookingId
      confirmReq <- ACL.buildConfirmReqV2 onInitRes
      void . withShortRetry $ CallBPP.confirmV2 onInitRes.bppUrl confirmReq onInitRes.merchant.id
      Redis.setExp confirmTriggeredKey ("1" :: Text) 86400
  pure (DPayment.FulfillmentSucceeded, Just bookingIdText, Nothing)

-- | Paytm EDC callback: update order status; for RideBooking+CHARGED run confirm only (no PaymentStatus build). Other types use orderStatusHandlerWithRefunds.
paytmEdcCallbackHandler :: PaytmEdcCallbackReq -> Flow AckResponse
paytmEdcCallbackHandler req = do
  paymentOrder <-
    QOrder.findByShortId (ShortId req.orderId)
      >>= maybe (QOrder.findById (Id req.orderId) >>= fromMaybeM (PaymentOrderNotFound req.orderId)) pure
  let lockKey = "PaytmEdcCallback:" <> getShortId paymentOrder.shortId
      mappedStatus = paytmStatusToTransactionStatus req.status
      paymentServiceType = fromMaybe DOrder.Normal paymentOrder.paymentServiceType
  Redis.whenWithLockRedis lockKey 60 $ do
    QOrder.updateStatus paymentOrder.id (fromMaybe "" req.txnId) mappedStatus
    if paymentServiceType == DOrder.RideBooking
      then
        when (mappedStatus == Payment.CHARGED) $ do
          eitherResult <- withTryCatch "paytmEdcCallback:fulfillment" $ confirmRideBookingFromPaymentOrder paymentOrder
          case eitherResult of
            Right (fulfillmentStatus, domainEntityId, domainTransactionId) ->
              void $ QOrder.updatePaymentFulfillmentStatus paymentOrder.id (Just fulfillmentStatus) domainEntityId domainTransactionId
            Left err -> logError $ "Paytm EDC fulfillment failed: " <> show err
      else do
        let paymentStatusResp =
              DPayment.PaymentStatus
                { orderId = paymentOrder.id,
                  orderShortId = paymentOrder.shortId,
                  status = mappedStatus,
                  bankErrorMessage = req.resultMsg,
                  bankErrorCode = req.resultCode,
                  isRetried = Nothing,
                  isRetargeted = Nothing,
                  retargetLink = Nothing,
                  refunds = [],
                  payerVpa = Nothing,
                  card = Nothing,
                  paymentMethodType = Nothing,
                  authIdCode = Nothing,
                  txnUUID = Nothing,
                  txnId = req.txnId,
                  effectAmount = Nothing,
                  offers = Nothing,
                  paymentServiceType = paymentOrder.paymentServiceType,
                  paymentFulfillmentStatus = paymentOrder.paymentFulfillmentStatus,
                  domainEntityId = paymentOrder.domainEntityId,
                  amount = paymentOrder.amount,
                  validTill = paymentOrder.validTill
                }
            fulfillmentHandler = mkFulfillmentHandler paymentServiceType (cast paymentOrder.merchantId) paymentOrder.id
        void $
          SPayment.orderStatusHandlerWithRefunds
            fulfillmentHandler
            paymentServiceType
            paymentOrder
            paymentOrder
            paymentStatusResp
  pure Ack

rideBookingOrderStatusHandler ::
  Id DOrder.PaymentOrder ->
  Id DM.Merchant ->
  DPayment.PaymentStatusResp ->
  Flow (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
rideBookingOrderStatusHandler orderId _merchantId paymentStatusResp = do
  status <- DPayment.getTransactionStatus paymentStatusResp
  case status of
    Payment.CHARGED -> do
      paymentOrder <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
      confirmRideBookingFromPaymentOrder paymentOrder
    _ -> pure (DPayment.FulfillmentPending, Nothing, Nothing)

mkFulfillmentHandler :: Payment.PaymentServiceType -> Id DM.Merchant -> Id DOrder.PaymentOrder -> SPayment.FulfillmentStatusHandler Flow
mkFulfillmentHandler paymentServiceType merchantId orderId paymentStatusResp = case paymentServiceType of
  DOrder.FRFSBooking -> FRFSTicketService.frfsOrderStatusHandler merchantId paymentStatusResp JMU.switchFRFSQuoteTierUtil
  DOrder.FRFSBusBooking -> FRFSTicketService.frfsOrderStatusHandler merchantId paymentStatusResp JMU.switchFRFSQuoteTierUtil
  DOrder.FRFSMultiModalBooking -> FRFSTicketService.frfsOrderStatusHandler merchantId paymentStatusResp JMU.switchFRFSQuoteTierUtil
  DOrder.FRFSPassPurchase -> do
    status <- DPayment.getTransactionStatus paymentStatusResp
    Pass.passOrderStatusHandler orderId merchantId status
  DOrder.ParkingBooking -> do
    status <- DPayment.getTransactionStatus paymentStatusResp
    ParkingBooking.parkingBookingOrderStatusHandler orderId merchantId status
  DOrder.BBPS -> do
    paymentFulfillStatus <- BBPS.bbpsOrderStatusHandler merchantId paymentStatusResp
    pure (paymentFulfillStatus, Nothing, Nothing)
  DOrder.RideBooking -> rideBookingOrderStatusHandler orderId merchantId paymentStatusResp
  _ -> pure (DPayment.FulfillmentPending, Nothing, Nothing)

mkOrderStatusCheckKey :: Text -> Payment.TransactionStatus -> Text
mkOrderStatusCheckKey orderId status = "lockKey:orderId:" <> orderId <> ":status" <> show status

stripeWebhookHandler,
  stripeTestWebhookHandler ::
    ShortId DM.Merchant ->
    Maybe Context.City ->
    Maybe Payment.PaymentServiceType ->
    Maybe Text ->
    Maybe Text ->
    RawByteString ->
    Flow AckResponse
stripeWebhookHandler = stripeWebhookHandler' Payment.Stripe
stripeTestWebhookHandler = stripeWebhookHandler' Payment.StripeTest

stripeWebhookHandler' ::
  Payment.PaymentService ->
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe Payment.PaymentServiceType ->
  Maybe Text ->
  Maybe Text ->
  RawByteString ->
  Flow AckResponse
stripeWebhookHandler' serviceName merchantShortId mbCity mbServiceType mbPlaceId mbSigHeader rawBytes = do
  paymentServiceConfig <- fetchPaymentServiceConfig merchantShortId mbCity mbServiceType mbPlaceId serviceName
  let checkDuplicatedEvent _eventId = pure False -- FIXME
  Stripe.serviceEventWebhook paymentServiceConfig checkDuplicatedEvent stripeWebhookAction mbSigHeader rawBytes

stripeWebhookAction ::
  PEInterface.ServiceEventResp ->
  Text ->
  Flow AckResponse
stripeWebhookAction resp respDump = do
  let stripeWebhookData = DPayment.mkStripeWebhookData resp.eventData
  case stripeWebhookData of
    DPayment.RefundWebhookData refundInfo -> do
      orderId <- (Id @DOrder.PaymentOrder <$>) $ refundInfo.orderId & fromMaybeM (InvalidRequest "orderId not found")
      refundsId <- (Id @DRefunds.Refunds <$>) $ refundInfo.refundsId & fromMaybeM (InvalidRequest "refundsId not found")
      Redis.whenWithLockRedis (DRidePayment.refundRequestProccessingKey orderId) 60 $ do
        void $ DPayment.stripeWebhookService resp respDump stripeWebhookData
        QRefundRequest.findByRefundsId (Just refundsId) >>= \case
          Nothing -> logInfo $ "No refund request found for update in webhook with refundsId: " <> refundsId.getId
          Just refundRequest -> do
            let updStatus = DRidePayment.castRefundRequestStatus refundInfo.status
            unless (refundRequest.status == updStatus) $ do
              QRefundRequest.updateRefundStatus updStatus refundRequest.id
              let updRefundRequest = refundRequest{status = updStatus}
              let rideId = cast @DOrder.PaymentOrder @DRide.Ride updRefundRequest.orderId
              QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
              -- Update refund invoice status using purpose from refund request
              let paymentPurpose = SPInvoice.refundPurposeToPaymentPurpose refundRequest.refundPurpose
                  invoiceStatus = SPInvoice.refundStatusToInvoiceStatus refundInfo.status
              QPaymentInvoiceExtra.updatePaymentStatusByRideIdAndTypeAndPurpose rideId DPI.REFUNDS paymentPurpose invoiceStatus
              Notify.notifyRefunds updRefundRequest
      pure Ack
    _ -> DPayment.stripeWebhookService resp respDump stripeWebhookData

----------------------------------------- wallet apis -----------------------------------------------------

postWalletRecharge ::
  (Id DP.Person, Id DM.Merchant) ->
  PaymentAPI.WalletRechargeReq ->
  Flow APISuccess
postWalletRecharge (personId, merchantId) req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound "personId")
  walletRewardPostingId <- generateGUID
  operationId <- generateShortId
  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = walletRewardPostingId,
            orderShortId = operationId.getShortId,
            amount = fromIntegral req.pointsAmount,
            customerId = person.id.getId,
            customerEmail = fromMaybe "growth@nammayatri.in" personEmail,
            customerPhone = personPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateEndDate = Nothing,
            mandateStartDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing,
            splitSettlementDetails = Nothing,
            basket = Nothing
          }

  let commonMerchantId = cast @DM.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      commonMerchantOperatingCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity person.merchantOperatingCityId
      createOrderCall = Payment.createOrder merchantId person.merchantOperatingCityId Nothing DOrder.Wallet (Just person.id.getId) person.clientSdkVersion Nothing
  mbPaymentOrderValidTill <- Payment.getPaymentOrderValidity merchantId person.merchantOperatingCityId Nothing DOrder.Wallet
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let createWalletCall = TWallet.createWallet merchantId person.merchantOperatingCityId
  mbOrderResp <- DPayment.createOrderService commonMerchantId (Just commonMerchantOperatingCityId) commonPersonId mbPaymentOrderValidTill Nothing DOrder.Wallet isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) False Nothing
  _ <- mbOrderResp & fromMaybeM (InternalError "Failed to create payment order")
  return Success

getWalletBalance ::
  (Id DP.Person, Id DM.Merchant) ->
  Flow Wallet.WalletBalanceData
getWalletBalance (personId, merchantId) = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personWallet <- QPersonWallet.findByPersonId personId.getId >>= fromMaybeM (InternalError $ "Person wallet not found for personId: " <> show personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  let walletBalanceReq = Wallet.WalletBalanceReq {customerId = personWallet.personId, requireHistory = Just True}
      walletBalanceCall = TWallet.walletBalance merchantId merchantOperatingCityId
  walletBalanceResp <- DPayment.walletBalanceService walletBalanceReq walletBalanceCall
  case walletBalanceResp.success of
    True -> do
      now <- getCurrentTime
      QPersonWallet.updateByPrimaryKey personWallet {DPersonWallet.pointsAmount = walletBalanceResp.walletData.pointsAmount, DPersonWallet.cashAmount = walletBalanceResp.walletData.cashAmount, DPersonWallet.expiredBalance = walletBalanceResp.walletData.expiredBalance, DPersonWallet.cashFromPointsRedemption = walletBalanceResp.walletData.cashFromPointsRedemption, DPersonWallet.usablePointsAmount = walletBalanceResp.walletData.usablePointsAmount, DPersonWallet.usableCashAmount = walletBalanceResp.walletData.usableCashAmount, DPersonWallet.updatedAt = now}
      return walletBalanceResp.walletData
    False -> throwError (InternalError $ "Failed to get wallet balance for personId: " <> show personId.getId)

-- redeemWallet :: (PaymentBeamFlow.BeamFlow m r, MonadThrow m) => Id.Id DP.Person -> HighPrecMoney -> m (Maybe DWalletRewardPosting.WalletPostingStatus)
-- redeemWallet personId pointsAmount = do
--   personWallet <- QPersonWallet.findByPersonId personId.getId >>= fromMaybeM (InternalError $ "Person wallet not found for personId: " <> show personId.getId)
--   walletRewardPosting <- QWalletRewardPosting.findByWalletIdAndStatus personWallet.id DWalletRewardPosting.NEW

--   walletRewardPostingId <- generateGUID
--   operationId <- generateShortId
--   now <- getCurrentTime

--   ------------- call payout service here ---------------

--   -- payoutConfig <- CPC.findByPrimaryKey merchOpCity vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchOpCity.getId)
--   -- payoutServiceName <- Payout.decidePayoutService (DEMSC.PayoutService TPayout.Juspay) person.clientSdkVersion person.merchantOperatingCityId
--   -- let entityName = DPayment.BACKLOG
--   --     createPayoutOrderCall = Payout.createPayoutOrder person.merchantId merchOpCity payoutServiceName (Just person.id.getId)
--   --     createPayoutOrderReq = DPayment.mkCreatePayoutOrderReq uid pendingAmount phoneNo person.email personId.getId payoutConfig.remark (Just person.firstName) vpa payoutConfig.orderType False
--   -- void $ DPayment.createPayoutService (cast person.merchantId) (Just $ cast merchOpCity) (cast personId) (Just statsIds) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall

--   let walletRewardPosting =
--         DWalletRewardPosting.WalletRewardPosting
--           { id = walletRewardPostingId,
--             shortId = operationId,
--             walletId = personWallet.id,
--             pointsAmount = pointsAmount,
--             cashAmount = HighPrecMoney {getHighPrecMoney = 0},
--             postingType = WalletInterface.REDEEM,
--             status = DWalletRewardPosting.NEW,
--             createdAt = now,
--             updatedAt = now,
--             merchantId = "",
--             merchantOperatingCityId = Nothing
--           }
--   QWalletRewardPosting.create walletRewardPosting
--   return (Just DWalletRewardPosting.NEW)
