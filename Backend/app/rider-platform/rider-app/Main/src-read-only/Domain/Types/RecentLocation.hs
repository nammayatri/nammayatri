{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RecentLocation (module Domain.Types.RecentLocation, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.RecentLocation as ReExport
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RecentLocation = RecentLocation
  { address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityType :: Domain.Types.RecentLocation.EntityType,
    frequency :: Kernel.Prelude.Int,
    fromStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromStopName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    stopLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EntityType = MULTIMODAL | BUS | METRO | TAXI deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EntityType))
