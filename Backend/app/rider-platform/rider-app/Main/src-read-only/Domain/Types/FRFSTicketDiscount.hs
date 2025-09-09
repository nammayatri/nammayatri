{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketDiscount where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSQuoteCategorySpec
import qualified Domain.Types.FRFSTicketCategoryMetadataConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicketDiscount = FRFSTicketDiscount
  { code :: Kernel.Prelude.Text,
    currency :: Kernel.Types.Common.Currency,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    ticketCategoryMetadataConfigId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig,
    value :: Domain.Types.FRFSQuoteCategorySpec.OfferedValue,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
