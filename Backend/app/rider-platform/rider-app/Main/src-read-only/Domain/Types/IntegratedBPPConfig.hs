{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.IntegratedBPPConfig (module Domain.Types.IntegratedBPPConfig, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig as ReExport
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.Extra.IntegratedBPPConfig
import qualified Kernel.Utils.TH
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data IntegratedBPPConfig
    = IntegratedBPPConfig {agencyKey :: Kernel.Prelude.Text,
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
                           updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON)
data PlatformType = MULTIMODAL | PARTNERORG | APPLICATION deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( ToParamSchema))
data ProviderConfig
    = EBIX Domain.Types.Extra.IntegratedBPPConfig.EBIXConfig
    | DIRECT Domain.Types.Extra.IntegratedBPPConfig.DIRECTConfig
    | CMRL Domain.Types.Extra.IntegratedBPPConfig.CMRLConfig
    | CMRLV2 Domain.Types.Extra.IntegratedBPPConfig.CMRLV2Config
    | ONDC Domain.Types.Extra.IntegratedBPPConfig.ONDCBecknConfig
    | CRIS Domain.Types.Extra.IntegratedBPPConfig.CRISConfig
    deriving (Generic, ( FromJSON), ( ToJSON), ( Eq), ( Show))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PlatformType))

$(Kernel.Utils.TH.mkToHttpInstanceForEnum (''PlatformType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''PlatformType))

