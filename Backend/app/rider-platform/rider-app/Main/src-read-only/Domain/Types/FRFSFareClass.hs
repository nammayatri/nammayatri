{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSFareClass where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH

data FRFSFareClass = FRFSFareClass
  { classCode :: Kernel.Prelude.Text,
    displayName :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSFareClass.FRFSFareClass,
    routeTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleServiceTierId :: Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier,
    vehicleTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.BusVehicleVariant,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
