module Tools.LoyaltyWallet
  ( loyaltyInfo,
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
