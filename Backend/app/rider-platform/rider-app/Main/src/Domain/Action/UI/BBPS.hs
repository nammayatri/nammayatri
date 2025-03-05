{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Domain.Action.UI.BBPS
  ( postBbpsSession,
    postBbpsCreateOrder,
    getBbpsGetOrderStatus,
    webhookHandlerBBPS,
    postBbpsPaymentStatus,
    postBbpsCrossCheckPayment,
    getBbpsOrders,
  )
where

import qualified API.Types.UI.BBPS
import qualified API.Types.UI.BBPS as API
import Control.Monad.Extra (maybeM)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Domain.Types.BBPS
import qualified Domain.Types.BBPS
import qualified Domain.Types.BBPS as DBBPS
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import Environment
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payment.Types as Payment
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Servant hiding (throwError)
import SharedLogic.External.BbpsService.Flow
import Storage.Beam.Payment ()
import qualified Storage.Queries.BBPS as QBBPS
import qualified Storage.Queries.BBPSConfig as QBBPSC
import qualified Storage.Queries.Person as QP
import Tools.Auth
import Tools.Error
import qualified Tools.Payment as Payment

postBbpsSession ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSSessionReq ->
    Environment.Flow Domain.Types.BBPS.BBPSSessionPayload
  )
postBbpsSession (_, merchantId) req = do
  bbpsConfig <- QBBPSC.findByMerchantId merchantId >>= fromMaybeM (InvalidRequest "BBPS config not found")
  sessionToken <- createBBPSJWT bbpsConfig req.mobileNumber req.deviceId
  case sessionToken of
    Left err -> throwError $ InternalError err
    Right tk -> pure $ BBPSSessionPayload tk

postBbpsCreateOrder ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSPaymentReq ->
    Environment.Flow Kernel.External.Payment.Interface.CreateOrderResp
  )
postBbpsCreateOrder (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  bbpsAmount <- (highPrecMoneyFromText req.billDetails.txnAmount) & fromMaybeM (InvalidRequest "Invalid amount")
  _bbpsConfig <- QBBPSC.findByMerchantId merchantId >>= fromMaybeM (InvalidRequest "BBPS not allowed for this merchant !")
  personEmail <- mapM decrypt person.email
  refShortId <- generateShortId
  now <- getCurrentTime
  let bbpsInfo =
        DBBPS.BBPS
          { DBBPS.refId = Kernel.Types.Id.Id req.bbpsTxnId,
            DBBPS.amount = bbpsAmount,
            DBBPS.billerId = req.billDetails.billerId,
            DBBPS.customerParams = req.billDetails.customerParams,
            DBBPS.customerMobileNumber = req.mobileNumber,
            DBBPS.customerId = personId,
            DBBPS.merchantId = person.merchantId,
            DBBPS.merchantOperatingCityId = person.merchantOperatingCityId,
            DBBPS.refShortId = refShortId,
            DBBPS.status = DBBPS.PENDING,
            DBBPS.transType = req.transType,
            DBBPS.paymentInformation = Nothing,
            DBBPS.paymentMode = Nothing,
            DBBPS.paymentTxnId = Nothing,
            DBBPS.errorMessage = Nothing,
            DBBPS.createdAt = now,
            DBBPS.updatedAt = now
          }
  isSplitEnabled <- Payment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing Payment.BBPS
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = req.bbpsTxnId,
            orderShortId = refShortId.getShortId,
            amount = bbpsAmount,
            customerId = personId.getId,
            customerEmail = fromMaybe "test@gmail.com" personEmail,
            customerPhone = req.mobileNumber,
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
            splitSettlementDetails = Payment.mkSplitSettlementDetails isSplitEnabled bbpsAmount []
          }
  let commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person personId
      createOrderCall = Payment.createOrder person.merchantId person.merchantOperatingCityId Nothing Payment.BBPS
  mCreateOrderRes <- DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast person.merchantOperatingCityId) commonPersonId createOrderReq createOrderCall
  case mCreateOrderRes of
    Just createOrderRes -> do
      QBBPS.create bbpsInfo
      return createOrderRes
    Nothing -> do
      throwError $ InternalError "Failed to create bbps order"

getBbpsGetOrderStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
  )
getBbpsGetOrderStatus (_, _) refIdTxt = do
  let refId = Kernel.Types.Id.Id refIdTxt
  bbpsInfo <- QBBPS.findByRefId refId >>= fromMaybeM (InvalidRequest "BBPS info not found")
  bbpsStatusHandler bbpsInfo

postBbpsCrossCheckPayment :: API.Types.UI.BBPS.BBPSServerReq -> Environment.Flow API.Types.UI.BBPS.BBPSServerResp
postBbpsCrossCheckPayment _req = do
  let refId = Kernel.Types.Id.Id _req.bbps_ref_id
  bbpsInfo <- QBBPS.findByRefId refId >>= fromMaybeM (InvalidRequest "BBPS info not found")
  let paidAmount = bbpsInfo.amount
  if
      | bbpsInfo.status == DBBPS.CONFIRMATION_PENDING -> do
        toBeCheckedAmount <- highPrecMoneyFromText _req.amount & fromMaybeM (InternalError "Invalid amount")
        if paidAmount == toBeCheckedAmount
          then do
            withBBPSLock refId $ QBBPS.updateStatusByRefId DBBPS.AWAITING_BBPS_CONFIRMATION refId
            return $ API.BBPSServerResp {status = API.SUCCESS, message = "SUCCESS"}
          else do
            logError $ "BBPS:" <> _req.bbps_ref_id <> ":Amount mismatch:Paid-" <> show paidAmount <> ":CrossCheck-" <> show toBeCheckedAmount
            return $ API.BBPSServerResp {status = API.FAILURE, message = "Amount mismatch"}
      | bbpsInfo.status `elem` [DBBPS.AWAITING_BBPS_CONFIRMATION, DBBPS.SUCCESS] ->
        return $ API.BBPSServerResp {status = API.SUCCESS, message = "SUCCESS"}
      | bbpsInfo.status `elem` [DBBPS.NEW, DBBPS.PENDING] -> do
        logError $ "BBPS:" <> _req.bbps_ref_id <> ":Confirmation asked but payment pending"
        return $ API.BBPSServerResp {status = API.FAILURE, message = "Payment Pending"}
      | otherwise ->
        return $ API.BBPSServerResp {status = API.FAILURE, message = "Payment Failed"}

postBbpsPaymentStatus :: API.Types.UI.BBPS.BBPSServerReq -> Environment.Flow API.Types.UI.BBPS.BBPSServerResp
postBbpsPaymentStatus _req = do
  let refId = Kernel.Types.Id.Id _req.bbps_ref_id
  bbpsInfo <- QBBPS.findByRefId refId >>= fromMaybeM (InvalidRequest "BBPS info not found")
  reqStatus <- _req.bbps_payment_status & fromMaybeM (InvalidRequest "Invalid BBPS status")
  if
      | bbpsInfo.status == DBBPS.AWAITING_BBPS_CONFIRMATION ->
        case reqStatus of
          API.SUCCESS -> do
            void $ withBBPSLock refId $ QBBPS.updateStatusByRefId DBBPS.SUCCESS refId
            return $ API.BBPSServerResp {status = API.SUCCESS, message = "SUCCESS"}
          API.FAILURE -> do
            void $ withBBPSLock refId $ QBBPS.updateStatusByRefId DBBPS.CONFIRMATION_FAILED refId
            -- void $ initiateAutoRefund bbpsInfo.merchantId bbpsInfo.merchantOperatingCityId bbpsInfo.refId bbpsInfo.refShortId bbpsInfo.amount
            return $ API.BBPSServerResp {status = API.SUCCESS, message = "Refund pending"}
      | bbpsInfo.status `elem` [DBBPS.REFUND_PENDING, DBBPS.REFUNDED, DBBPS.FAILED] ->
        return $ API.BBPSServerResp {status = API.FAILURE, message = "Payment Failed/Refund Initiated"}
      | otherwise -> return $ API.BBPSServerResp {status = API.FAILURE, message = "Payment Confirmation Pending"}

getBbpsOrders ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentStatus ->
    Environment.Flow [API.Types.UI.BBPS.BBPSInfoAPIRes]
  )
getBbpsOrders (mbPersonId, merchantId) mbLimit mbOffset mbActive mbStatus = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person Id not found")
  let limit = fromMaybe 10 mbLimit
  allBBPSInfos <- QBBPS.getAllBBPSOrders limit mbOffset mbActive mbStatus personId merchantId
  return $ map mkBBPSInfoApiRes allBBPSInfos

mkBBPSInfoApiRes :: DBBPS.BBPS -> API.BBPSInfoAPIRes
mkBBPSInfoApiRes DBBPS.BBPS {..} = API.BBPSInfoAPIRes {billDetails = API.BBPSBillDetails {txnAmount = highPrecMoneyToText amount, ..}, ..}

webhookHandlerBBPS :: Kernel.Types.Id.ShortId DPaymentOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Environment.Flow API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
webhookHandlerBBPS orderShortId _merchantId = do
  logDebug $ "bbps order bap webhookc call" <> orderShortId.getShortId
  order <- QOrder.findByShortId orderShortId >>= fromMaybeM (PaymentOrderNotFound orderShortId.getShortId)
  bbpsInfo <- QBBPS.findByRefId (Kernel.Types.Id.cast order.id) >>= fromMaybeM (InvalidRequest "BBPS info not found")
  bbpsStatusHandler bbpsInfo

bbpsStatusHandler :: DBBPS.BBPS -> Environment.Flow API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
bbpsStatusHandler bbpsInfo = do
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person bbpsInfo.customerId
      orderStatusCall = Payment.orderStatus bbpsInfo.merchantId bbpsInfo.merchantOperatingCityId Nothing Payment.BBPS
      oldResp =
        API.BBPSPaymentStatusAPIRes
          { status = bbpsInfo.status,
            paymentInformation = bbpsInfo.paymentInformation,
            paymentMode = bbpsInfo.paymentMode,
            paymentTxnId = bbpsInfo.paymentTxnId,
            errorMessage = bbpsInfo.errorMessage
          }
  if
      | bbpsInfo.status `elem` [DBBPS.NEW, DBBPS.PENDING] -> do
        paymentStatusResp <- DPayment.orderStatusService commonPersonId (Kernel.Types.Id.cast bbpsInfo.refId) orderStatusCall
        maybeM
          (pure oldResp)
          ( \(bbpsStatus, paymentInfo, payMode, txnUUID) -> do
              when (bbpsInfo.status /= bbpsStatus) $ do
                if bbpsStatus == DBBPS.CONFIRMATION_PENDING
                  then withBBPSLock bbpsInfo.refId $ QBBPS.updatePaymentInformationAndStatusByRefId txnUUID payMode paymentInfo bbpsStatus bbpsInfo.refId
                  else withBBPSLock bbpsInfo.refId $ QBBPS.updateStatusByRefId bbpsStatus bbpsInfo.refId
              return $
                API.BBPSPaymentStatusAPIRes
                  { status = bbpsStatus,
                    paymentTxnId = txnUUID,
                    paymentInformation = paymentInfo,
                    paymentMode = payMode,
                    errorMessage = bbpsInfo.errorMessage
                  }
          )
          (pure $ makePaymentInformation paymentStatusResp)
      | bbpsInfo.status `elem` [DBBPS.CONFIRMATION_PENDING, DBBPS.FAILED, DBBPS.SUCCESS, DBBPS.REFUNDED, DBBPS.REFUND_FAILED] -> return oldResp
      | bbpsInfo.status `elem` [DBBPS.AWAITING_BBPS_CONFIRMATION] -> return oldResp -- Extra state might be needed later to handle some cases -- we can call bbps status api here..
      | bbpsInfo.status `elem` [DBBPS.REFUND_PENDING, DBBPS.CONFIRMATION_FAILED] -> do
        fork "Trigger Refund data updation" $ do
          refunds <- QRefunds.findAllByOrderId (Kernel.Types.Id.cast bbpsInfo.refId)
          let isAnyRefundPending = any (\refund -> refund.status `elem` [Payment.REFUND_PENDING, Payment.MANUAL_REVIEW]) refunds
              isRefundSuccess = any (\refund -> refund.status == Payment.REFUND_SUCCESS) refunds
              allRefundFailed = all (\refund -> refund.status == Payment.REFUND_FAILURE) refunds
          if
              | isRefundSuccess -> void $ withBBPSLock bbpsInfo.refId $ QBBPS.updateStatusByRefId DBBPS.REFUNDED bbpsInfo.refId
              | isAnyRefundPending -> void $ do
                when (bbpsInfo.status /= DBBPS.REFUND_PENDING) $ withBBPSLock bbpsInfo.refId $ QBBPS.updateStatusByRefId DBBPS.REFUND_PENDING bbpsInfo.refId
                DPayment.orderStatusService commonPersonId (Kernel.Types.Id.cast bbpsInfo.refId) orderStatusCall
              | allRefundFailed -> void $ withBBPSLock bbpsInfo.refId $ QBBPS.updateStatusByRefId DBBPS.REFUND_FAILED bbpsInfo.refId
              | otherwise -> pure ()
        return oldResp
      | otherwise -> return oldResp

makePaymentInformation :: DPayment.PaymentStatusResp -> Maybe (DBBPS.BBPSPaymentStatus, Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag], Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentMode, Kernel.Prelude.Maybe Kernel.Prelude.Text)
makePaymentInformation pResp =
  case pResp of
    DPayment.PaymentStatus {..} ->
      let bbpsStatus = txnStatusToBBPSStatus status
          paymentMode = paymentMethodType >>= \pmt -> getPaymentMode pmt card
          paymentInformation =
            paymentMode >>= \case
              DBBPS.UPI -> payerVpa <&> (\vpa -> [Tag {name = "VPA", value = vpa}])
              DBBPS.Debit_Card -> makeCardTags
              DBBPS.Credit_Card -> makeCardTags
              DBBPS.Others -> Nothing
       in pure (bbpsStatus, paymentInformation, paymentMode, pResp.txnUUID)
    _ -> Nothing
  where
    makeCardTags :: Maybe [DBBPS.Tag]
    makeCardTags = (\last4Dig code -> [Tag {name = "CardNum|AuthCode", value = (last4Dig <> "|" <> code)}]) <$> (pResp.card >>= (.lastFourDigits)) <*> pResp.authIdCode

getPaymentMode :: Text -> Maybe Payment.CardInfo -> Maybe DBBPS.BBPSPaymentMode
getPaymentMode paymentMethodType mbCardInfo = case paymentMethodType of
  "UPI" -> Just DBBPS.UPI
  "CARD" -> getCardType
  _ -> Nothing
  where
    getCardType =
      mbCardInfo >>= (.cardType) >>= \case
        "DEBIT" -> Just DBBPS.Debit_Card
        "CREDIT" -> Just DBBPS.Credit_Card
        _ -> Nothing

txnStatusToBBPSStatus :: Payment.TransactionStatus -> DBBPS.BBPSPaymentStatus
txnStatusToBBPSStatus = \case
  Payment.NEW -> DBBPS.NEW
  Payment.PENDING_VBV -> DBBPS.PENDING
  Payment.CHARGED -> DBBPS.CONFIRMATION_PENDING
  Payment.AUTHENTICATION_FAILED -> DBBPS.FAILED
  Payment.AUTHORIZATION_FAILED -> DBBPS.FAILED
  Payment.JUSPAY_DECLINED -> DBBPS.FAILED
  Payment.AUTHORIZING -> DBBPS.PENDING
  Payment.COD_INITIATED -> DBBPS.REFUNDED
  Payment.STARTED -> DBBPS.PENDING
  Payment.AUTO_REFUNDED -> DBBPS.REFUNDED
  Payment.CLIENT_AUTH_TOKEN_EXPIRED -> DBBPS.FAILED
  Payment.CANCELLED -> DBBPS.FAILED

-- initiateAutoRefund :: Kernel.Types.Id.Id Merchant.Merchant -> Kernel.Types.Id.Id MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.Id DBBPS.BBPS -> Kernel.Types.Id.ShortId DBBPS.BBPS -> HighPrecMoney -> Environment.Flow ()
-- initiateAutoRefund personMerchantId merchantOperatingCityId orderRefId orderShortId amount = do
--   refundId <- generateGUID
--   let autoRefundReq =
--         Payment.AutoRefundReq
--           { orderId = orderShortId.getShortId,
--             amount = amount,
--             requestId = refundId
--           }
--       commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant personMerchantId
--       createRefundCall = Payment.refundOrder personMerchantId merchantOperatingCityId Nothing Payment.BBPS
--   _ <- DPayment.refundService (autoRefundReq, Kernel.Types.Id.Id refundId) commonMerchantId createRefundCall
--   QBBPS.updateStatusByRefId DBBPS.REFUND_PENDING orderRefId

withBBPSLock :: Kernel.Types.Id.Id DBBPS.BBPS -> Environment.Flow () -> Environment.Flow ()
withBBPSLock id func = Redis.whenWithLockRedis (bbpsLockKey id.getId) 60 func

bbpsLockKey :: Text -> Text
bbpsLockKey id = "BBPS:RefId-" <> id
