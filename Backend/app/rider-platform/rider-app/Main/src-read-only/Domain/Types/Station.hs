{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Station where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationType
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH

data Station = Station
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    possibleTypes :: Kernel.Prelude.Maybe [Domain.Types.StationType.StationType],
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
