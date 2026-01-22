{-# OPTIONS_GHC -Wno-orphans #-}

module Handler where

import API
import App.Types
import Data.Aeson (decode, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.UUID.V4 as UUIDV4
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName (..))
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Juspay.Types as Juspay
import qualified Kernel.Storage.Beam.SystemConfigs as BeamSC
import Kernel.Types.Beckn.Ack (AckResponse (..))
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common (throwError)
import Kernel.Utils.Error.FlowHandling (withFlowHandlerAPI')
import Kernel.Utils.Logging (logInfo)
import qualified Lib.Payment.Domain.Action as PaymentAction
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrderOffer as DOffer
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTxn
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Lib.Payment.Storage.Beam.PaymentOrder as BeamPO
import qualified Lib.Payment.Storage.Beam.PaymentOrderOffer as BeamOffer
import qualified Lib.Payment.Storage.Beam.PaymentOrderSplit as BeamPOS
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as BeamPT
import qualified Lib.Payment.Storage.Beam.PayoutOrder as BeamPOO
import qualified Lib.Payment.Storage.Beam.PayoutTransaction as BeamPOT
import qualified Lib.Payment.Storage.Beam.PersonWallet as BeamPW
import qualified Lib.Payment.Storage.Beam.Refunds as BeamRF
import qualified Lib.Payment.Storage.Beam.WalletRewardPosting as BeamWRP
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrderOffer as QOffer
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QTxn
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types.Status (statusCode)
import Servant ((:<|>) (..))
import Servant.Client (showBaseUrl)
import Servant.Server (ServerError (..), err400, err500)

instance HasSchemaName BeamSC.SystemConfigsT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamPO.PaymentOrderT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamOffer.PaymentOrderOfferT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamPOS.PaymentOrderSplitT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamPT.PaymentTransactionT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamRF.RefundsT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamPOO.PayoutOrderT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamPOT.PayoutTransactionT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamWRP.WalletRewardPostingT where
  schemaName _ = "atlas_app"

instance HasSchemaName BeamPW.PersonWalletT where
  schemaName _ = "atlas_app"

server :: FlowServer API
server = externalPaymentHandler :<|> internalOrderStatusHandler

externalPaymentHandler ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  PaymentAction.PaymentStatusResp ->
  FlowHandler Value
externalPaymentHandler merchantShortId mCity mServiceType paymentStatusResp =
  withFlowHandlerAPI' $
    case paymentStatusResp of
      PaymentAction.PaymentStatus {..} -> do
        logInfo $
          "Mock Payment: Processing payment for orderId: "
            <> getId orderId

        now <- liftIO Time.getCurrentTime
        eventId <-
          liftIO $
            ("evt_" <>)
              . T.pack
              . show
              <$> UUIDV4.nextRandom

        AppEnv {juspayWebhookBaseUrl = envJuspayUrl} <- ask
        let juspayBaseUrl = showBaseUrl envJuspayUrl
            baseWebhookUrl =
              juspayBaseUrl
                <> "/"
                <> T.unpack merchantShortId
                <> "/service/juspay/payment"

            cityParam =
              maybe "" (\c -> "?city=" <> T.unpack c) mCity

            serviceTypeParam =
              maybe
                ""
                (\st -> (if null cityParam then "?" else "&") <> "serviceType=%22" <> T.unpack st <> "%22")
                mServiceType

            webhookUrl = baseWebhookUrl <> cityParam <> serviceTypeParam

        mOrder <- QOrder.findByShortId orderShortId
        (mTxn, dateCreated) <- case mOrder of
          Just order -> do
            txn <- QTxn.findNewTransactionByOrderId order.id
            pure (txn, Just order.createdAt)
          Nothing -> pure (Nothing, Nothing)

        let paymentMethod = mTxn >>= DTxn.paymentMethod
            paymentGatewayResponse = mTxn >>= DTxn.juspayResponse >>= parsePaymentGatewayResponse
            respMessage = mTxn >>= DTxn.respMessage
            respCode = mTxn >>= DTxn.respCode
            gatewayReferenceId = mTxn >>= DTxn.gatewayReferenceId
            splitSettlementResponse = mTxn >>= DTxn.splitSettlementResponse
            amountRefunded = Just $ sum $ map (.amount) $ filter (\r -> r.status == Payment.REFUND_SUCCESS) refunds

            webhookPayload =
              buildJuspayWebhookPayload
                orderShortId
                status
                bankErrorMessage
                bankErrorCode
                (map toJuspayRefundsData refunds)
                payerVpa
                (fmap toJuspayCardInfo card)
                paymentMethodType
                txnUUID
                txnId
                effectAmount
                (fmap (map toJuspayOffer) offers)
                amount
                now
                eventId
                paymentMethod
                paymentGatewayResponse
                respMessage
                respCode
                gatewayReferenceId
                dateCreated
                amountRefunded
                splitSettlementResponse

        logInfo $ "Mock Payment: Webhook Payload: " <> T.pack (show webhookPayload)

        logInfo $
          "Mock Payment: Calling Juspay Sandbox Webhook: "
            <> T.pack webhookUrl

        manager <- liftIO TLS.newTlsManager
        initialRequest <- liftIO $ HTTP.parseRequest webhookUrl

        let request =
              initialRequest
                { HTTP.method = "POST",
                  HTTP.requestBody =
                    HTTP.RequestBodyLBS $ encode webhookPayload,
                  HTTP.requestHeaders =
                    [ ("Content-Type", "application/json"),
                      ("Authorization", "Basic Y3VtdGE6Y3VtdGFAMTIz")
                    ]
                }

        response <- liftIO $ HTTP.httpLbs request manager

        let responseBody = HTTP.responseBody response
            respStatusCode =
              statusCode $ HTTP.responseStatus response

        logInfo $
          "Mock Payment: Juspay response status: "
            <> T.pack (show respStatusCode)

        logInfo $
          "Mock Payment: Juspay response body: "
            <> TE.decodeUtf8 (LBS.toStrict responseBody)

        if respStatusCode >= 200 && respStatusCode < 300
          then case eitherDecode responseBody of
            Right jsonResp -> pure jsonResp
            Left _ -> pure $ toJSON Ack
          else do
            -- Return the actual webhook error response to the caller
            let baseErr = if respStatusCode >= 400 && respStatusCode < 500 then err400 else err500
            throwM $
              baseErr
                { errHTTPCode = respStatusCode,
                  errBody = responseBody,
                  errHeaders = [("Content-Type", "application/json")]
                }
      _ ->
        throwError $
          InternalError "Expected PaymentStatus constructor"

buildJuspayWebhookPayload ::
  ShortId a ->
  Payment.TransactionStatus ->
  Maybe Text ->
  Maybe Text ->
  [Juspay.RefundsData] ->
  Maybe Text ->
  Maybe Juspay.CardInfo ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe HighPrecMoney ->
  Maybe [Juspay.Offer] ->
  HighPrecMoney ->
  UTCTime ->
  Text ->
  Maybe Text ->
  Maybe Juspay.PaymentGatewayResponse ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Maybe HighPrecMoney ->
  Maybe Payment.SplitSettlementResponse ->
  Juspay.WebhookReq
buildJuspayWebhookPayload
  orderShortId
  transactionStatus
  bankErrorMessage
  bankErrorCode
  refunds
  payerVpa
  card
  paymentMethodType
  txnUUID
  txnId
  effectAmount
  offers
  transactionAmount
  now
  eventId
  paymentMethod
  paymentGatewayResponse
  respMessage
  respCode
  gatewayReferenceId
  dateCreated
  amountRefunded
  splitSettlementResponse =
    Juspay.WebhookReq
      { id = eventId,
        date_created = now,
        event_name = statusToPaymentStatus transactionStatus,
        content =
          Juspay.OrderAndNotificationStatusContent
            { order =
                Just
                  Juspay.OrderData
                    { order_id = getShortId orderShortId,
                      txn_uuid = txnUUID,
                      txn_id = txnId,
                      status_id = Just 21,
                      event_name =
                        Just $
                          statusToPaymentStatus transactionStatus,
                      status = transactionStatus,
                      payment_method_type = paymentMethodType,
                      payment_method = paymentMethod,
                      payment_gateway_response = paymentGatewayResponse,
                      resp_message = respMessage,
                      resp_code = respCode,
                      gateway_reference_id = gatewayReferenceId,
                      amount =
                        realToFrac $
                          getHighPrecMoney transactionAmount,
                      currency = INR,
                      date_created = dateCreated,
                      mandate = Nothing,
                      payer_vpa = payerVpa,
                      bank_error_code = bankErrorCode,
                      bank_error_message = bankErrorMessage,
                      upi = Nothing,
                      card = card,
                      metadata = Nothing,
                      additional_info = Nothing,
                      links = Nothing,
                      amount_refunded = fmap (realToFrac . getHighPrecMoney) amountRefunded,
                      refunds = Just refunds,
                      split_settlement_response = splitSettlementResponse >>= toJuspaySplitSettlementResponse,
                      effective_amount =
                        fmap
                          (realToFrac . getHighPrecMoney)
                          effectAmount,
                      offers = offers
                    },
              mandate = Nothing,
              notification = Nothing,
              txn = Nothing
            }
      }

internalOrderStatusHandler ::
  Text ->
  FlowHandler Payment.OrderStatusResp
internalOrderStatusHandler orderShortId =
  withFlowHandlerAPI' $ do
    mOrder <- QOrder.findByShortId (ShortId orderShortId)
    case mOrder of
      Just order -> do
        let DOrder.PaymentOrder {id = orderId, shortId = orderShortId'} = order
        mTxn <- QTxn.findNewTransactionByOrderId orderId
        refunds <- QRefunds.findAllByOrderId orderShortId'
        offers <- QOffer.findByPaymentOrder orderId
        buildOrderStatusResp order mTxn refunds offers
      Nothing ->
        throwError $ InternalError ("Order not found: " <> orderShortId)

buildOrderStatusResp ::
  (MonadFlow m) =>
  DOrder.PaymentOrder ->
  Maybe DTxn.PaymentTransaction ->
  [DRefunds.Refunds] ->
  [DOffer.PaymentOrderOffer] ->
  m Payment.OrderStatusResp
buildOrderStatusResp order mTxn refunds offers = do
  let txnId = mTxn >>= DTxn.txnId
      txnUUID = mTxn >>= DTxn.txnUUID
      respMessage = mTxn >>= DTxn.respMessage
      respCode = mTxn >>= DTxn.respCode
      gatewayRefId = mTxn >>= DTxn.gatewayReferenceId
      DOrder.PaymentOrder
        { status = orderStatus,
          shortId = orderShortIdVal,
          amount = orderAmount,
          createdAt = orderCreatedAt,
          isRetried = orderIsRetried,
          isRetargeted = orderIsRetargeted,
          retargetLink = orderRetargetLink,
          bankErrorMessage = orderBankErrorMessage,
          bankErrorCode = orderBankErrorCode,
          effectAmount = orderEffectAmount,
          clientAuthTokenExpiry = orderClientAuthTokenExpiry
        } = order

  pure $
    Payment.OrderStatusResp
      { eventName =
          Just $
            statusToEventName orderStatus,
        orderShortId =
          getShortId orderShortIdVal,
        transactionUUID = txnUUID,
        txnId = txnId,
        transactionStatusId =
          statusToId orderStatus,
        transactionStatus =
          orderStatus,
        paymentMethodType =
          mTxn >>= DTxn.paymentMethodType,
        paymentMethod = mTxn >>= DTxn.paymentMethod,
        paymentGatewayResponse = mTxn >>= DTxn.juspayResponse >>= (decode . LBS.fromStrict . TE.encodeUtf8),
        respMessage = respMessage,
        respCode = respCode,
        gatewayReferenceId = gatewayRefId,
        bankErrorMessage =
          orderBankErrorMessage,
        bankErrorCode =
          orderBankErrorCode,
        amount =
          orderAmount,
        currency = INR,
        dateCreated =
          Just orderCreatedAt,
        isRetriedOrder =
          Just orderIsRetried,
        isRetargetedOrder =
          Just orderIsRetargeted,
        retargetPaymentLink =
          orderRetargetLink,
        retargetPaymentLinkExpiry = orderClientAuthTokenExpiry,
        amountRefunded = Just totalRefundedAmount,
        refunds = map toPaymentRefundsData refunds,
        payerVpa = mTxn >>= DTxn.paymentMethod,
        upi = Nothing,
        card = Nothing,
        splitSettlementResponse = mTxn >>= DTxn.splitSettlementResponse,
        effectiveAmount =
          orderEffectAmount,
        offers = Just $ map toPaymentOffer offers
      }
  where
    totalRefundedAmount =
      foldr
        ( \refund acc ->
            if refund.status == Payment.REFUND_SUCCESS
              then acc + refund.refundAmount
              else acc
        )
        0
        refunds

    toPaymentOffer :: DOffer.PaymentOrderOffer -> Payment.Offer
    toPaymentOffer offer =
      Payment.Offer
        { offerId = Just offer.offer_id,
          offerCode = Just offer.offer_code,
          status = fromMaybe Payment.OFFER_INITIATED (readMaybe (T.unpack offer.status))
        }

    toPaymentRefundsData :: DRefunds.Refunds -> Payment.RefundsData
    toPaymentRefundsData refund =
      Payment.RefundsData
        { idAssignedByServiceProvider = refund.idAssignedByServiceProvider,
          amount = refund.refundAmount,
          status = refund.status,
          errorMessage = refund.errorMessage,
          errorCode = refund.errorCode,
          initiatedBy = refund.initiatedBy,
          requestId = getShortId refund.shortId,
          arn = refund.arn
        }

statusToPaymentStatus :: Payment.TransactionStatus -> Juspay.PaymentStatus
statusToPaymentStatus = \case
  Payment.CHARGED -> Juspay.ORDER_SUCCEEDED
  Payment.AUTO_REFUNDED -> Juspay.ORDER_REFUNDED
  _ -> Juspay.ORDER_FAILED

statusToEventName :: Payment.TransactionStatus -> Payment.PaymentStatus
statusToEventName = \case
  Payment.CHARGED -> Payment.ORDER_SUCCEEDED
  Payment.AUTO_REFUNDED -> Payment.ORDER_REFUNDED
  _ -> Payment.ORDER_FAILED

statusToId :: Payment.TransactionStatus -> Int
statusToId = \case
  Payment.NEW -> 10
  Payment.PENDING_VBV -> 20
  Payment.CHARGED -> 21
  Payment.AUTHENTICATION_FAILED -> 22
  Payment.AUTHORIZATION_FAILED -> 23
  Payment.JUSPAY_DECLINED -> 24
  Payment.AUTHORIZING -> 25
  Payment.COD_INITIATED -> 26
  Payment.STARTED -> 27
  Payment.AUTO_REFUNDED -> 28
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> 29
  Payment.CANCELLED -> 30

toJuspayRefundStatus :: Payment.RefundStatus -> Juspay.RefundStatus
toJuspayRefundStatus = \case
  Payment.REFUND_PENDING -> Juspay.REFUND_PENDING
  Payment.REFUND_FAILURE -> Juspay.REFUND_FAILURE
  Payment.REFUND_SUCCESS -> Juspay.REFUND_SUCCESS
  Payment.MANUAL_REVIEW -> Juspay.MANUAL_REVIEW
  Payment.REFUND_CANCELED -> Juspay.REFUND_FAILURE
  Payment.REFUND_REQUIRES_ACTION -> Juspay.REFUND_PENDING

toJuspayRefundsData :: Payment.RefundsData -> Juspay.RefundsData
toJuspayRefundsData Payment.RefundsData {..} =
  Juspay.RefundsData
    { id = idAssignedByServiceProvider,
      amount = realToFrac amount,
      status = toJuspayRefundStatus status,
      error_message = errorMessage,
      error_code = errorCode,
      initiated_by = initiatedBy,
      unique_request_id = requestId,
      arn = arn
    }

toJuspayCardInfo :: Payment.CardInfo -> Juspay.CardInfo
toJuspayCardInfo Payment.CardInfo {..} =
  Juspay.CardInfo
    { card_type = cardType,
      last_four_digits = lastFourDigits
    }

toJuspayOffer :: Payment.Offer -> Juspay.Offer
toJuspayOffer Payment.Offer {..} =
  Juspay.Offer
    { offer_id = offerId,
      offer_code = offerCode,
      status = status
    }

parsePaymentGatewayResponse :: Text -> Maybe Juspay.PaymentGatewayResponse
parsePaymentGatewayResponse val = decode (LBS.fromStrict $ TE.encodeUtf8 val)

toJuspaySplitSettlementResponse :: Payment.SplitSettlementResponse -> Maybe Juspay.SplitSettlementResponse
toJuspaySplitSettlementResponse resp =
  Just $
    Juspay.SplitSettlementResponse
      { split_details = fmap (map toJuspaySplitDetailsResponse) resp.splitDetails,
        split_applied = resp.splitApplied
      }

toJuspaySplitDetailsResponse :: Payment.SplitDetailsResponse -> Juspay.SplitDetailsResponse
toJuspaySplitDetailsResponse detail =
  Juspay.SplitDetailsResponse
    { sub_vendor_id = detail.subVendorId,
      amount = detail.amount,
      merchant_commission = detail.merchantCommission,
      gateway_sub_account_id = detail.gatewaySubAccountId,
      epg_txn_id = detail.epgTxnId,
      unique_split_id = detail.uniqueSplitId
    }
