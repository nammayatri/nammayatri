module Domain.Types.Merchant.MerchantServiceUsageConfig where

import Beckn.External.Maps.Types (MapsService)
import Beckn.External.SMS.Types
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { merchantId :: Id Merchant,
    getDistances :: MapsService,
    getRoutes :: MapsService,
    snapToRoad :: MapsService,
    getPlaceName :: MapsService,
    getPlaceDetails :: MapsService,
    autoComplete :: MapsService,
    sendSMS :: SmsService,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)
