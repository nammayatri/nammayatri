{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RecentLocation (module Domain.Types.RecentLocation, module ReExport) where
import Kernel.Prelude
import Data.Aeson
import Domain.Types.Extra.RecentLocation as ReExport
import qualified Kernel.Types.Common
import qualified Kernel.External.Maps.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data RecentLocation
    = RecentLocation {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      createdAt :: Kernel.Prelude.UTCTime,
                      entityType :: Domain.Types.RecentLocation.EntityType,
                      fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                      frequency :: Kernel.Prelude.Int,
                      fromGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      fromLatLong :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
                      fromStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      id :: Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation,
                      merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                      riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                      routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      toGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      toLatLong :: Kernel.External.Maps.Types.LatLong,
                      toStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                      updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data EntityType = MULTIMODAL | SUBWAY | BUS | METRO | TAXI deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EntityType))

