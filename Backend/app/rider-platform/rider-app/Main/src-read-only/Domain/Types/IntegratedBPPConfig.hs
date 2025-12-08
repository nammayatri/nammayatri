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
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data IntegratedBPPConfig = IntegratedBPPConfig
  { agencyKey :: Kernel.Prelude.Text,
    domain :: Kernel.Prelude.Text,
    feedKey :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    isTicketValidOnMultipleRoutes :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    platformType :: Domain.Types.IntegratedBPPConfig.PlatformType,
    providerConfig :: Domain.Types.IntegratedBPPConfig.ProviderConfig,
    providerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PlatformType = MULTIMODAL | PARTNERORG | APPLICATION deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ProviderConfig
  = EBIX Domain.Types.Extra.IntegratedBPPConfig.EBIXConfig
  | DIRECT Domain.Types.Extra.IntegratedBPPConfig.DIRECTConfig
  | CMRL Domain.Types.Extra.IntegratedBPPConfig.CMRLConfig
  | ONDC Domain.Types.Extra.IntegratedBPPConfig.ONDCBecknConfig
  | CRIS Domain.Types.Extra.IntegratedBPPConfig.CRISConfig
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''PlatformType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PlatformType)
