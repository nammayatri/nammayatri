{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSCancellationConfig where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data FRFSCancellationConfig = FRFSCancellationConfig
  { cancellationChargeType :: Domain.Types.FRFSCancellationConfig.CancellationChargeType,
    cancellationChargeValue :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSCancellationConfig.FRFSCancellationConfig,
    maxMinutesBeforeDeparture :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minMinutesBeforeDeparture :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: BecknV2.FRFS.Enums.VehicleCategory
  }
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

data CancellationChargeType = PERCENTAGE | FLAT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CancellationChargeType))

$(mkHttpInstancesForEnum (''CancellationChargeType))
