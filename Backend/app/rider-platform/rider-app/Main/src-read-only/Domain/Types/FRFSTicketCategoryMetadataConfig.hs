{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.FRFSTicketCategoryMetadataConfig where
import Kernel.Prelude
import Data.Aeson
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSQuoteCategorySpec
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified BecknV2.FRFS.Enums
import qualified Tools.Beam.UtilsTH



data FRFSTicketCategoryMetadataConfig
    = FRFSTicketCategoryMetadataConfig {category :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
                                        categoryOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                                        code :: Kernel.Prelude.Text,
                                        description :: Kernel.Prelude.Text,
                                        domainCategoryValue :: Domain.Types.FRFSQuoteCategorySpec.OfferedValue,
                                        id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig,
                                        merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                        merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                                        title :: Kernel.Prelude.Text,
                                        tnc :: Kernel.Prelude.Text,
                                        vehicleCategory :: BecknV2.FRFS.Enums.VehicleCategory,
                                        createdAt :: Kernel.Prelude.UTCTime,
                                        updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



