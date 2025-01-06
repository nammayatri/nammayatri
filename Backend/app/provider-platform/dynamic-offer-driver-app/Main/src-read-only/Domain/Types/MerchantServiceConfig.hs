{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantServiceConfig (module Domain.Types.MerchantServiceConfig, module ReExport) where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantServiceConfig as ReExport
import qualified Domain.Types.Extra.MerchantServiceConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantServiceConfigD (s :: UsageSafety) = MerchantServiceConfig
  { createdAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    serviceConfig :: Domain.Types.Extra.MerchantServiceConfig.ServiceConfigD s,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

type MerchantServiceConfig = MerchantServiceConfigD 'Safe

instance FromJSON (MerchantServiceConfigD 'Unsafe)

instance ToJSON (MerchantServiceConfigD 'Unsafe)

instance FromJSON (MerchantServiceConfigD 'Safe)

instance ToJSON (MerchantServiceConfigD 'Safe)
