{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteStopFare where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH

data RouteStopFare = RouteStopFare
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    endStopCode :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    routeCode :: Kernel.Prelude.Text,
    startStopCode :: Kernel.Prelude.Text,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleServiceTierId :: Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    vehicleVariant :: BecknV2.FRFS.Enums.VehicleVariant,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
