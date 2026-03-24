{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSFarePolicy where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.FRFSTicketCategoryMetadataConfig
import qualified Kernel.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data FRFSFarePolicy
    = FRFSFarePolicy {_type :: Domain.Types.FRFSFarePolicy.FRFSFarePolicyType,
                      applicableDiscountIds :: [Kernel.Types.Id.Id Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig],
                      cessCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                      description :: Kernel.Prelude.Text,
                      id :: Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy,
                      merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                      merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                      createdAt :: Kernel.Prelude.UTCTime,
                      updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data FRFSFarePolicyType = MatrixBased | StageBased deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FRFSFarePolicyType))

