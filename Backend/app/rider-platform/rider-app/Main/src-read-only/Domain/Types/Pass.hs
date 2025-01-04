{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Pass where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassType
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Pass = Pass
  { amount :: Kernel.Types.Common.HighPrecMoney,
    benefit :: Kernel.Prelude.Maybe Domain.Types.Pass.Benefit,
    code :: Kernel.Prelude.Text,
    days :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passTypeId :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    purchaseEligibilityJsonLogic :: [Data.Aeson.Value],
    redeemEligibilityJsonLogic :: [Data.Aeson.Value],
    savings :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    trips :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data Benefit = Discount | Custom Kernel.Prelude.Text deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Benefit)
