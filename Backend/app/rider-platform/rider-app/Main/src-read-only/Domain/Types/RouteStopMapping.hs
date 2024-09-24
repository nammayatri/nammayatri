{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteStopMapping where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route
import qualified Domain.Types.Station
import Kernel.Prelude hiding (sequence)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import qualified Tools.Beam.UtilsTH

data RouteStopMapping = RouteStopMapping
  { id :: Kernel.Types.Id.Id Domain.Types.RouteStopMapping.RouteStopMapping,
    routeId :: Kernel.Types.Id.Id Domain.Types.Route.Route,
    sequence :: Kernel.Prelude.Int,
    stopId :: Kernel.Types.Id.Id Domain.Types.Station.Station,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    vehicleType :: Domain.Types.Station.FRFSVehicleType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
