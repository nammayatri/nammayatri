{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantClientConfig (module Domain.Types.MerchantClientConfig, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.MerchantClientConfig as ReExport
import qualified Kernel.Types.Version
import qualified Domain.Types.Extra.MerchantClientConfig
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data MerchantClientConfig
    = MerchantClientConfig {clientOS :: Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType,
                            clientServiceConfig :: Domain.Types.Extra.MerchantClientConfig.ClientServiceConfig,
                            createdAt :: Kernel.Prelude.UTCTime,
                            packageId :: Kernel.Prelude.Text,
                            updatedAt :: Kernel.Prelude.UTCTime,
                            merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                            merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)}
    deriving (Generic, Show)



