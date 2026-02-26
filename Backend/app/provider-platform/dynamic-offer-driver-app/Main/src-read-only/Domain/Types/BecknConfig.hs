{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BecknConfig where
import Kernel.Prelude
import Data.Aeson
import qualified Servant.Client.Core
import qualified Kernel.Types.Id
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data BecknConfig
    = BecknConfig {buyerFinderFee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   collectedBy :: Domain.Types.BecknConfig.PaymentCollectedBy,
                   domain :: Kernel.Prelude.Text,
                   gatewayUrl :: Servant.Client.Core.BaseUrl,
                   id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
                   multimodalOnSearchTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onCancelTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onConfirmTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onInitTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onSearchTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onSelectTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onStatusTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onTrackTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   onUpdateTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                   paymentParamsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   ppfEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                   registryUrl :: Servant.Client.Core.BaseUrl,
                   settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   settlementWindow :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   staticTermsUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
                   subscriberId :: Kernel.Prelude.Text,
                   subscriberUrl :: Servant.Client.Core.BaseUrl,
                   uniqueKeyId :: Kernel.Prelude.Text,
                   vehicleCategory :: BecknV2.OnDemand.Enums.VehicleCategory,
                   merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                   merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                   createdAt :: Kernel.Prelude.UTCTime,
                   updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data PaymentCollectedBy = BAP | BPP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentCollectedBy))

