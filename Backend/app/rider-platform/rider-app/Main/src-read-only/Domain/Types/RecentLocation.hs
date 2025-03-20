{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RecentLocation where

import Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RecentLocation = RecentLocation
  { createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityType :: Domain.Types.RecentLocation.EntityType,
    id :: Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EntityType = MULTIMODAL | BUS | METRO | NAMMA_YATRI deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''EntityType))
