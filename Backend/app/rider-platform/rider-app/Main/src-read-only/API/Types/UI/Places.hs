{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Places where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.MultiModal.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data PlacesRequest = PlacesRequest
  { merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    userLat :: Kernel.Prelude.Double,
    userLon :: Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlacesResponse = PlacesResponse {popularLocations :: [PopularLocation], recentLocations :: [RecentLocation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PopularLocation = PopularLocation
  { address :: Data.Text.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    multimodalRoutes :: Kernel.External.MultiModal.Interface.Types.MultiModalResponse,
    name :: Data.Text.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    type_ :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RecentLocation = RecentLocation
  { address :: Data.Text.Text,
    distance :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    mode :: Domain.Types.RecentLocation.EntityType,
    multimodalRoutes :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalResponse,
    name :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
