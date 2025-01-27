{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Route where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH

data Route = Route
  { code :: Kernel.Prelude.Text,
    color :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endPoint :: Kernel.External.Maps.Types.LatLong,
    id :: Kernel.Types.Id.Id Domain.Types.Route.Route,
    longName :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    polyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    roundRouteCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shortName :: Kernel.Prelude.Text,
    startPoint :: Kernel.External.Maps.Types.LatLong,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleType :: Domain.Types.VehicleCategory.VehicleCategory,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)
