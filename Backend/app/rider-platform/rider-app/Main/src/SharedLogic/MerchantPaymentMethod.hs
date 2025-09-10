module SharedLogic.MerchantPaymentMethod where

import Domain.Types.MerchantPaymentMethod

mkPaymentMethodInfo :: MerchantPaymentMethod -> PaymentMethodInfo
mkPaymentMethodInfo MerchantPaymentMethod {..} = PaymentMethodInfo {..}
