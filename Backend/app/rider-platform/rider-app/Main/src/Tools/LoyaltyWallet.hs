module Tools.LoyaltyWallet
  ( loyaltyInfo,
    svpWalletPosting,
    svpWalletBalance,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Wallet.Interface as Wallet
import qualified Kernel.External.Wallet.Interface.Types as WalletTypes
import qualified Kernel.External.Wallet.Juspay.Config as JuspayWallet
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)

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
  let serviceName = DMSC.MultiModalPaymentService Payment.Juspay
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, merchantId = merchantId.getId, serviceName = Just serviceName})
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "MultiModalPayment" (show Payment.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.MultiModalPaymentServiceConfig paymentCfg ->
      case paymentCfg of
        Payment.JuspayConfig juspayCfg -> do
          let loyaltyCfg = JuspayWallet.LoyaltyCfg {baseUrl = juspayCfg.url, apiKey = juspayCfg.apiKey}
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
        _ -> throwError $ InternalError "Unsupported payment config for loyalty info"
    _ -> throwError $ InternalError "Unknown Service Config"

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
