{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AddOnConfig where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data AddOnConfig = AddOnConfig
  { addOnType :: Domain.Types.AddOnConfig.AddOnType,
    createdAt :: Kernel.Prelude.UTCTime,
    descriptorName :: Kernel.Prelude.Text,
    descriptorShortDesc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.AddOnConfig.AddOnConfig,
    maxQuantity :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    metadata :: Kernel.Prelude.Maybe [Domain.Types.AddOnConfig.AddOnMetadata],
    pricePerQuantity :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: [Domain.Types.Common.ServiceTierType]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AddOnData = AddOnData {configId :: Kernel.Types.Id.Id Domain.Types.AddOnConfig.AddOnConfig, selectedQuantity :: Kernel.Prelude.Int} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data AddOnMetadata = AddOnMetadata {title :: Kernel.Prelude.Text, value :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data AddOnType = RIDER_INSURANCE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AddOnType)

$(mkHttpInstancesForEnum ''AddOnType)
