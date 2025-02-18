{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.StopInformation where

import Data.Aeson
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data StopInformation = StopInformation
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.StopInformation.StopInformation,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    stopEndLatLng :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    stopLocId :: Kernel.Types.Id.Id Domain.Types.Location.Location,
    stopOrder :: Kernel.Prelude.Int,
    stopStartLatLng :: Kernel.External.Maps.LatLong,
    updatedAt :: Kernel.Prelude.UTCTime,
    waitingTimeEnd :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    waitingTimeStart :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
