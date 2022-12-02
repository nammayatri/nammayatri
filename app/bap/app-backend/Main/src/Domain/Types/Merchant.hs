module Domain.Types.Merchant where

import Beckn.External.FCM.Types
import Beckn.Prelude
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Domain.Types.Common

data MerchantD (s :: UsageSafety) = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    name :: Text,
    exoPhone :: Maybe Text,
    exoPhoneCountryCode :: Maybe Text,
    fcmConfig :: FCMConfig,
    geofencingConfig :: GeofencingConfig,
    gatewayUrl :: BaseUrl,
    registryUrl :: BaseUrl
  }
  deriving (Generic)

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)
