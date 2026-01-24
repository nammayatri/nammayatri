module SharedLogic.MerchantPaymentMethod where

import Domain.Types (BknPaymentParams)
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.MerchantPaymentMethod
import qualified Domain.Types.RiderConfig as DRiderConfig
import Kernel.Prelude
import Kernel.Utils.Common (decodeFromText)

mkPaymentMethodInfo :: MerchantPaymentMethod -> PaymentMethodInfo
mkPaymentMethodInfo MerchantPaymentMethod {..} = PaymentMethodInfo {..}

-- Returns Beckn payment params when applicable based on merchant payment method info
mkBknPaymentParams :: Maybe PaymentMethodInfo -> DBC.BecknConfig -> DRiderConfig.RiderConfig -> Maybe BknPaymentParams
mkBknPaymentParams mbPaymentMethodInfo bapConfig riderConfig = do
  if riderConfig.enableOnlinePaymentRide == Just True
    then case mbPaymentMethodInfo of
      Just paymentMethodInfo ->
        if paymentMethodInfo.paymentInstrument == Cash || paymentMethodInfo.paymentInstrument == BoothOnline
          then Nothing
          else decodeFromText =<< bapConfig.paymentParamsJson
      Nothing -> Nothing
    else decodeFromText =<< bapConfig.paymentParamsJson
