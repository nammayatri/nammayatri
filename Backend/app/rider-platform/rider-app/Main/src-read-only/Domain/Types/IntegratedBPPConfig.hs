{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IntegratedBPPConfig where

import qualified BecknV2.FRFS.Enums
import qualified BecknV2.OnDemand.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Base64
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IntegratedBPPConfig = IntegratedBPPConfig
  { domain :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    qrGeneratedBy :: BecknV2.FRFS.Enums.Network,
    qrGenerationKey :: Kernel.Prelude.Maybe Kernel.Types.Base64.Base64,
    qrVerificationKey :: Kernel.Prelude.Maybe Kernel.Types.Base64.Base64,
    qrVerifiedBy :: BecknV2.FRFS.Enums.Network,
    vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)
