{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BecknConfig where
import Kernel.Prelude
import Data.Aeson
import qualified BecknV2.FRFS.Enums
import qualified Servant.Client.Core
import qualified Kernel.Types.Id
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data BecknConfig
    = BecknConfig {bapIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   buyerFinderFee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   cancelTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   collectedBy :: BecknV2.FRFS.Enums.Network,
                   confirmBufferTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   confirmTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   domain :: Kernel.Prelude.Text,
                   gatewayUrl :: Servant.Client.Core.BaseUrl,
                   id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
                   initTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   paymentParamsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   ppfEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                   ratingTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   registryUrl :: Servant.Client.Core.BaseUrl,
                   searchTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   selectTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   settlementWindow :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   staticTermsUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
                   statusTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   subscriberId :: Kernel.Prelude.Text,
                   subscriberUrl :: Servant.Client.Core.BaseUrl,
                   trackTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   uniqueKeyId :: Kernel.Prelude.Text,
                   vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
                   merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                   merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                   createdAt :: Kernel.Prelude.UTCTime,
                   updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, FromJSON, ToJSON)



