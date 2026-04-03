{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RouteTripStopMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Data.Time
import qualified Kernel.External.Maps.Types
import qualified Domain.Types.VehicleCategory
import qualified Tools.Beam.UtilsTH



data RouteTripStopMapping
    = RouteTripStopMapping {enabled :: Kernel.Prelude.Bool,
                            merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                            merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                            providerCode :: Kernel.Prelude.Text,
                            routeCode :: Kernel.Prelude.Text,
                            scheduledArrival :: Data.Time.TimeOfDay,
                            scheduledDay :: Data.Time.DayOfWeek,
                            scheduledDeparture :: Data.Time.TimeOfDay,
                            stopCode :: Kernel.Prelude.Text,
                            stopName :: Kernel.Prelude.Text,
                            stopPoint :: Kernel.External.Maps.Types.LatLong,
                            stopSequenceNum :: Kernel.Prelude.Int,
                            tripCode :: Kernel.Prelude.Text,
                            tripSequenceNum :: Kernel.Prelude.Int,
                            vehicleType :: Domain.Types.VehicleCategory.VehicleCategory,
                            createdAt :: Kernel.Prelude.UTCTime,
                            updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, FromJSON, ToJSON)



