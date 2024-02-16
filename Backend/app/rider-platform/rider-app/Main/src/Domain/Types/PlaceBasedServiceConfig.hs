{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PlaceBasedServiceConfig where

import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PlaceBasedServiceConfigD (s :: UsageSafety) = PlaceBasedServiceConfig
  { merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    serviceConfig :: Domain.Types.Merchant.MerchantServiceConfig.ServiceConfigD s,
    placeId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type PlaceBasedServiceConfig = PlaceBasedServiceConfigD 'Safe

instance FromJSON (PlaceBasedServiceConfigD 'Unsafe)

instance ToJSON (PlaceBasedServiceConfigD 'Unsafe)
