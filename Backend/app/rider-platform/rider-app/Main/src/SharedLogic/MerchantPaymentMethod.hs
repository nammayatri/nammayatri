module SharedLogic.MerchantPaymentMethod where

import Domain.Types (BknPaymentParams)
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.MerchantPaymentMethod
import Kernel.Prelude
import Kernel.Utils.Common (decodeFromText)

mkPaymentMethodInfo :: MerchantPaymentMethod -> PaymentMethodInfo
mkPaymentMethodInfo MerchantPaymentMethod {..} = PaymentMethodInfo {..}

-- Returns Beckn payment params when applicable based on merchant payment method info
mkBknPaymentParams :: Maybe PaymentMethodInfo -> DBC.BecknConfig -> Maybe BknPaymentParams
mkBknPaymentParams mbPaymentMethodInfo bapConfig =
  case mbPaymentMethodInfo of
    Just paymentMethodInfo ->
      if paymentMethodInfo.paymentInstrument == Cash
        then Nothing
        else decodeFromText =<< bapConfig.paymentParamsJson
    Nothing -> Nothing
