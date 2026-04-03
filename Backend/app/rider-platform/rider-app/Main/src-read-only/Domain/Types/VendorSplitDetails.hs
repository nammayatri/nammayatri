{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.VendorSplitDetails where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Extra.VendorSplitDetails
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data VendorSplitDetails
    = VendorSplitDetails {id :: Kernel.Types.Id.Id Domain.Types.VendorSplitDetails.VendorSplitDetails,
                          includeInSplit :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                          integratedBPPConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
                          splitShare :: Kernel.Prelude.Maybe Domain.Types.Extra.VendorSplitDetails.SplitShare,
                          splitType :: Domain.Types.VendorSplitDetails.SplitType,
                          vendorId :: Data.Text.Text,
                          merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                          createdAt :: Kernel.Prelude.UTCTime,
                          updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data SplitType = FIXED | FLEXIBLE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SplitType))

