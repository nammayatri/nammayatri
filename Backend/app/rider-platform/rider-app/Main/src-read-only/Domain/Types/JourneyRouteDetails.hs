{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.JourneyRouteDetails where

import Data.Aeson
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Route
import qualified Domain.Types.Station
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Tools.Beam.UtilsTH

data JourneyRouteDetails = JourneyRouteDetails
  { frequency :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds,
    fromStationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station),
    id :: Kernel.Types.Id.Id Domain.Types.JourneyRouteDetails.JourneyRouteDetails,
    lineColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lineColorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    platformNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Route.Route),
    routeLongName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchId :: Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch,
    subLegOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    toStationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Station.Station),
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
