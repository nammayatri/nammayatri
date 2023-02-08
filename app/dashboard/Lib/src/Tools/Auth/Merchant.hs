{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tools.Auth.Merchant (merchantAccessCheck, CheckedShortId) where

import qualified Domain.Types.Merchant as DMerchant
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (ToHttpApiData)

merchantAccessCheck :: MonadFlow m => ShortId DMerchant.Merchant -> ShortId DMerchant.Merchant -> m (CheckedShortId DMerchant.Merchant)
merchantAccessCheck (ShortId userMerchantId) (ShortId merchantId) = do
  unless (userMerchantId == merchantId) $ throwError AccessDenied
  pure $ CheckedShortId merchantId

-- CheckedShortId constructor should not be exported for type safety
newtype CheckedShortId domain = CheckedShortId Text
  deriving newtype (ToHttpApiData, ToSchema)
