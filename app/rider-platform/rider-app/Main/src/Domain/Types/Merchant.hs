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
    exoPhones :: [Text],
    exoPhoneCountryCode :: Maybe Text,
    fcmConfig :: FCMConfig,
    geofencingConfig :: GeofencingConfig,
    gatewayUrl :: BaseUrl,
    registryUrl :: BaseUrl,
    driverOfferBaseUrl :: BaseUrl,
    driverOfferApiKey :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

type Merchant = MerchantD 'Safe

instance FromJSON (MerchantD 'Unsafe)

instance ToJSON (MerchantD 'Unsafe)
