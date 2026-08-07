{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonUsageStats where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PersonUsageStats = PersonUsageStats
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PersonUsageStats.PersonUsageStats,
    lastPurchasedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    passTypeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassType.PassType),
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    productType :: Domain.Types.PersonUsageStats.FRFSProductType,
    purchaseCount :: Kernel.Prelude.Int,
    staticPersonId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    vehicleType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, (Show), (Eq), (ToJSON), (FromJSON))

data FRFSProductType = TICKET | PASS deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''FRFSProductType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''FRFSProductType))
