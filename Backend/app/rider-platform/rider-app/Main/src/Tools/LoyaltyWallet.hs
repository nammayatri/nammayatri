{-# LANGUAGE TypeApplications #-}

module Tools.LoyaltyWallet
  ( loyaltyInfo,
    createTxn,
    getJuspayLoyaltyCfg,
    deductSvpFare,
    SvpDeductionParams (..),
    svpWalletPosting,
    svpWalletBalance,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Environment (Flow)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Juspay.Config as PaymentJuspay
import qualified Kernel.External.SVP.Types as SVP
import qualified Kernel.External.Wallet.Interface as Wallet
import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import qualified Kernel.External.Wallet.Juspay.Config as JuspayWallet
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.Queries.Person as QP
import qualified Tools.Payment as TPayment

-- | Fetch loyalty/wallet info — same source the consumer app wallet screen uses.
loyaltyInfo ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Text ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m WalletTypes.LoyaltyInfoResponse
loyaltyInfo customerId merchantId merchantOperatingCityId = do
  (juspayCfg, loyaltyCfg) <- getJuspayLoyaltyCfg (DMSC.MultiModalPaymentService Payment.Juspay) merchantId merchantOperatingCityId
  let req =
        WalletTypes.LoyaltyInfoRequest
          { customer = WalletTypes.CustomerRequest {customerId = customerId},
            order =
              Just $
                WalletTypes.OrderRequest
                  { merchantId = juspayCfg.merchantId,
                    currency = "INR",
                    amount = "0"
                  }
          }
  Wallet.loyaltyInfo loyaltyCfg req

-- | Debit loyalty points by hitting Juspay POST /txns directly with a caller-built request.
createTxn ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  WalletTypes.CreateTxnRequest ->
  m WalletTypes.CreateTxnResponse
createTxn merchantId merchantOperatingCityId req = do
  (_, loyaltyCfg) <- getJuspayLoyaltyCfg (DMSC.MultiModalPaymentService Payment.Juspay) merchantId merchantOperatingCityId
  Wallet.createTxn loyaltyCfg req

-- | Fetch the Juspay payment service config for a given service name and build a LoyaltyCfg from it.
-- Returns both the underlying JuspayCfg (for fields like merchantId) and the derived LoyaltyCfg
-- (for direct API calls). Supports both MultiModalPaymentService and JuspayWalletService variants.
getJuspayLoyaltyCfg ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m (PaymentJuspay.JuspayCfg, JuspayWallet.LoyaltyCfg)
getJuspayLoyaltyCfg serviceName merchantId merchantOperatingCityId = do
  msc <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName})
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId (show serviceName) (show Payment.Juspay))
  juspayCfg <- case msc.serviceConfig of
    DMSC.MultiModalPaymentServiceConfig (Payment.JuspayConfig cfg) -> pure cfg
    DMSC.JuspayWalletServiceConfig (Payment.JuspayConfig cfg) -> pure cfg
    _ -> throwError $ InternalError $ "Unsupported service config for loyalty/wallet operations: " <> show serviceName
  let loyaltyCfg = JuspayWallet.LoyaltyCfg {baseUrl = juspayCfg.url, apiKey = juspayCfg.apiKey}
  pure (juspayCfg, loyaltyCfg)

-- | Deduct fare from the MultiModal wallet (same wallet the consumer app displays).
svpWalletPosting ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  WalletTypes.WalletPostingReq ->
  m WalletTypes.WalletPostingResp
svpWalletPosting merchantId merchantOperatingCityId req =
  runWithMultiModalConfig Wallet.walletPosting merchantId merchantOperatingCityId req

-- | Read live balance from the MultiModal wallet (same wallet the consumer app displays).
svpWalletBalance ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  WalletTypes.WalletBalanceReq ->
  m WalletTypes.WalletBalanceResp
svpWalletBalance merchantId merchantOperatingCityId req =
  runWithMultiModalConfig Wallet.walletBalance merchantId merchantOperatingCityId req

-- | Run any wallet operation using the MultiModalPaymentService config.
runWithMultiModalConfig ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  (Payment.PaymentServiceConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithMultiModalConfig func merchantId merchantOperatingCityId req = do
  let serviceName = DMSC.MultiModalPaymentService Payment.Juspay
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName})
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "MultiModalPayment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.MultiModalPaymentServiceConfig paymentCfg -> func paymentCfg req
    _ -> throwError $ InternalError "Unknown Service Config for SVP wallet"

-- | Inputs for an SVP fare deduction. Called from a CMRL gate-exit webhook —
-- amount and customerId come from the webhook payload; the rest are caller-provided.
data SvpDeductionParams = SvpDeductionParams
  { customerId :: Text,
    amount :: HighPrecMoney
  }
  deriving stock (Show, Generic)

-- | Fetch the SVP config (programId, burnOptionId, gatewayId, paymentMethod, paymentMethodType)
-- from MerchantServiceConfig for the given merchant + operating city.
getSVPCfg ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m SVP.SVPConfig
getSVPCfg merchantId merchantOperatingCityId = do
  let serviceName = DMSC.SVPService SVP.Juspay
  msc <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName})
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "SVP" (show SVP.Juspay))
  case msc.serviceConfig of
    DMSC.SVPServiceConfig cfg -> pure cfg
    _ -> throwError $ InternalError "Unexpected service config for SVP"

-- | Create a Juspay order (session API) and immediately charge it against the user's
-- loyalty wallet via POST /txns. Failures (createOrder, createTxn, non-CHARGED status)
-- are logged and swallowed — this is invoked from a webhook with no user-facing recovery path.
deductSvpFare :: SvpDeductionParams -> Flow ()
deductSvpFare params = do
  let personId = Id params.customerId :: Id DP.Person
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound params.customerId)

  (juspayCfg, loyaltyCfg) <-
    getJuspayLoyaltyCfg
      (DMSC.PassPaymentService Payment.Juspay)
      person.merchantId
      person.merchantOperatingCityId

  svpCfg <- getSVPCfg person.merchantId person.merchantOperatingCityId

  personEmail <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt

  paymentOrderId <- generateGUID
  operationId <- generateShortId
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = paymentOrderId,
            orderShortId = operationId.getShortId,
            amount = params.amount,
            customerId = params.customerId,
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
            basket = Nothing,
            paymentRules = Nothing,
            autoRefundPostSuccess = Nothing
          }
      commonMerchantId = cast @DM.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = cast @DP.Person @DPayment.Person personId
      commonMerchantOpCityId = cast @DMOC.MerchantOperatingCity @DPayment.MerchantOperatingCity person.merchantOperatingCityId
      createOrderCall = TPayment.createOrder person.merchantId person.merchantOperatingCityId Nothing DOrder.FRFSPassPurchase (Just params.customerId) person.clientSdkVersion Nothing

  mbPaymentOrderValidTill <- TPayment.getPaymentOrderValidity person.merchantId person.merchantOperatingCityId Nothing DOrder.FRFSPassPurchase
  isMetroTestTransaction <- asks (.isMetroTestTransaction)

  mbOrderResp <-
    DPayment.createOrderService commonMerchantId (Just commonMerchantOpCityId) commonPersonId mbPaymentOrderValidTill Nothing DOrder.FRFSPassPurchase isMetroTestTransaction createOrderReq createOrderCall Nothing False Nothing
  case mbOrderResp of
    Nothing ->
      logError $ "[SVP:Deduct] createOrderService returned Nothing for customerId=" <> params.customerId
    Just orderResp -> do
      let pointsStr = T.pack $ show (floor (getHighPrecMoney params.amount) :: Int)
          txnReq =
            WalletTypes.CreateTxnRequest
              { orderId = orderResp.order_id,
                merchantId = juspayCfg.merchantId,
                paymentMethodType = svpCfg.paymentMethodType,
                paymentMethod = svpCfg.paymentMethod,
                gatewayId = svpCfg.gatewayId,
                loyaltyOsDetails =
                  WalletTypes.LoyaltyOsDetails
                    { programId = svpCfg.programId,
                      burnOptionsSelected =
                        [ WalletTypes.BurnOptionSelected
                            { id_ = svpCfg.burnOptionId,
                              points = pointsStr
                            }
                        ]
                    },
                format = "json"
              }
      txnResult <- try @_ @SomeException $ Wallet.createTxn loyaltyCfg txnReq
      case txnResult of
        Left err ->
          logError $ "[SVP:Deduct] createTxn failed order=" <> orderResp.order_id <> " err=" <> show err
        Right resp ->
          if resp.status == "CHARGED"
            then logInfo $ "[SVP:Deduct] success customerId=" <> params.customerId <> " order=" <> resp.orderId <> " txn=" <> resp.txnId
            else logError $ "[SVP:Deduct] non-CHARGED status=" <> resp.status <> " order=" <> resp.orderId <> " txn=" <> resp.txnId
