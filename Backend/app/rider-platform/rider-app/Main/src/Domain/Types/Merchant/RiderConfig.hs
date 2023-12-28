module Domain.Types.Merchant.RiderConfig where

import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.Id

data RiderConfig = RiderConfig
  { merchantOperatingCityId :: Id MerchantOperatingCity,
    enableLocalPoliceSupport :: Bool,
    localPoliceNumber :: Maybe Text,
    enableSupportForSafety :: Bool,
    videoFileSizeUpperLimit :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
