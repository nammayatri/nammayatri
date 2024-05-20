module Domain.Action.UI.Merchant where

import Domain.Types.Merchant
import Kernel.Prelude

makeMerchantAPIEntity :: Merchant -> MerchantAPIEntity
makeMerchantAPIEntity Merchant {..} =
  MerchantAPIEntity
    { contactNumber = fromMaybe "Unknown" $ mobileCountryCode <> mobileNumber,
      ..
    }
