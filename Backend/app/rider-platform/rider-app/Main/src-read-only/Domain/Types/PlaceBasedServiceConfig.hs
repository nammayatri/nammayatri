{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PlaceBasedServiceConfig where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PlaceBasedServiceConfigD (s :: UsageSafety) = PlaceBasedServiceConfig
  { merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    placeId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    serviceConfig :: Domain.Types.MerchantServiceConfig.ServiceConfigD s,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type PlaceBasedServiceConfig = PlaceBasedServiceConfigD 'Safe

instance FromJSON (PlaceBasedServiceConfigD 'Unsafe)

instance ToJSON (PlaceBasedServiceConfigD 'Unsafe)

instance FromJSON (PlaceBasedServiceConfigD 'Safe)

instance ToJSON (PlaceBasedServiceConfigD 'Safe)
