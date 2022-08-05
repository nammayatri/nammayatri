module Domain.Types.Merchant where

import Beckn.External.FCM.Types
import Beckn.Prelude
import Beckn.Types.Geofencing
import Beckn.Types.Id

data Merchant = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    exoPhone :: Maybe Text,
    exoPhoneCountryCode :: Maybe Text,
    fcmConfig :: FCMConfig,
    geofencingConfig :: GeofencingConfig,
    gatewayUrl :: BaseUrl,
    registryUrl :: BaseUrl
  }
  deriving (Generic)
