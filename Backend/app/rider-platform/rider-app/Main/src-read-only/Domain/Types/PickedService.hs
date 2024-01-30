{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PickedService where

import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PickedService = PickedService
  { autoComplete :: Kernel.External.Maps.Types.MapsService,
    getDistances :: Kernel.External.Maps.Types.MapsService,
    getDistancesForCancelRide :: Kernel.External.Maps.Types.MapsService,
    getPickupRoutes :: Kernel.External.Maps.Types.MapsService,
    getPlaceDetails :: Kernel.External.Maps.Types.MapsService,
    getPlaceName :: Kernel.External.Maps.Types.MapsService,
    getRoutes :: Kernel.External.Maps.Types.MapsService,
    getTripRoutes :: Kernel.External.Maps.Types.MapsService,
    searchRequestId :: Kernel.Types.Id.Id Domain.Types.PickedService.PickedService,
    snapToRoad :: Kernel.External.Maps.Types.MapsService,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
