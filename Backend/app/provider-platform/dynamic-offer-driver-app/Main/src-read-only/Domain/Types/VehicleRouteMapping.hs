{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleRouteMapping where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleRouteMapping = VehicleRouteMapping
  { allowEndingMidRoute :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeCode :: Data.Text.Text,
    vehicleClass :: Data.Text.Text,
    vehicleColor :: Data.Text.Text,
    vehicleModel :: Data.Text.Text,
    vehicleNumber :: Data.Text.Text,
    vehicleServiceTierType :: Domain.Types.Common.ServiceTierType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
