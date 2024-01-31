{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PickedServices where

import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PickedServices = PickedServices
  { autoComplete :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getPickupRoutes :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getPlaceName :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getRoutes :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.MapsService,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    searchRequestId :: Kernel.Types.Id.Id Domain.Types.PickedServices.PickedServices,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
