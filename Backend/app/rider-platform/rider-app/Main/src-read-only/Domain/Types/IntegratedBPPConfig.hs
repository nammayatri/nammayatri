{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IntegratedBPPConfig (module Domain.Types.IntegratedBPPConfig, module ReExport) where

import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig as ReExport
import qualified Domain.Types.Extra.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IntegratedBPPConfig = IntegratedBPPConfig
  { domain :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    providerConfig :: Domain.Types.IntegratedBPPConfig.ProviderConfig,
    vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

data ProviderConfig
  = EBIX Domain.Types.Extra.IntegratedBPPConfig.EBIXConfig
  | CUMTA Domain.Types.Extra.IntegratedBPPConfig.CUMTAConfig
  | CMRL Domain.Types.Extra.IntegratedBPPConfig.CMRLConfig
  deriving (Generic, FromJSON, ToJSON, Eq)
