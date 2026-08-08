{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VasBannerConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data VasBannerConfig = VasBannerConfig
  { createdAt :: Kernel.Prelude.UTCTime,
    deepLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.VasBannerConfig.VasBannerConfig,
    imageUrl :: Kernel.Prelude.Text,
    linkType :: Domain.Types.VasBannerConfig.VasBannerLinkType,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    priority :: Kernel.Prelude.Int,
    subtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    whatsappTemplateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data VasBannerLinkType = Service | SmartFinance | WhatsApp | ExternalUrl | Other deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''VasBannerLinkType))

$(mkHttpInstancesForEnum (''VasBannerLinkType))
