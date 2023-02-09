module Domain.Types.Merchant.MerchantServiceUsageConfig where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Kernel.External.Maps.Types (MapsService)
import Kernel.External.SMS.Types
import Kernel.Prelude
import Kernel.Types.Id

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { merchantId :: Id Merchant,
    getDistances :: MapsService,
    getEstimatedPickupDistances :: MapsService,
    getRoutes :: MapsService,
    snapToRoad :: MapsService,
    getPlaceName :: MapsService,
    getPlaceDetails :: MapsService,
    autoComplete :: MapsService,
    smsProvidersPriorityList :: [SmsService],
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)
