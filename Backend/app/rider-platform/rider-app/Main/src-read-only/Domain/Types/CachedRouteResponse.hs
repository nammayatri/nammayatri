{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CachedRouteResponse where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CachedRouteResponse = CachedRouteResponse
  { avoidToll :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    dropGeohash :: Kernel.Prelude.Text,
    duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    hourOfDay :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.CachedRouteResponse.CachedRouteResponse,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    pickupGeohash :: Kernel.Prelude.Text,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routes :: [Kernel.External.Maps.Types.RouteInfo],
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
