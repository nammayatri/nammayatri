module Domain.Types.Merchant where

import Domain.Types.Common
import Kernel.External.FCM.Types
import Kernel.Prelude
import Kernel.Types.Geofencing
import Kernel.Types.Id

data MerchantD (s :: UsageSafety) = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    name :: Text,
    exoPhone :: Maybe Text,
    exoPhoneCountryCode :: Maybe Text,
    fcmConfig :: FCMConfig,
    geofencingConfig :: GeofencingConfig,
    gatewayUrl :: BaseUrl,
    registryUrl :: BaseUrl,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)
