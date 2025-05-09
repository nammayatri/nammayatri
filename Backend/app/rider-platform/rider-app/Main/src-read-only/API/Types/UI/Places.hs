{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Places where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.RecentLocation
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.MultiModal.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data MultiModalLocation = MultiModalLocation
  { address :: Data.Text.Text,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fromStationCode :: Kernel.Prelude.Maybe Data.Text.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    mode :: Kernel.Prelude.Maybe Domain.Types.RecentLocation.EntityType,
    multimodalRoutes :: Kernel.Prelude.Maybe Kernel.External.MultiModal.Interface.Types.MultiModalResponse,
    name :: Data.Text.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    recentLocationId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation),
    routeCode :: Kernel.Prelude.Maybe Data.Text.Text,
    toStationCode :: Kernel.Prelude.Maybe Data.Text.Text,
    type_ :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlacesRequest = PlacesRequest {integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig, userLat :: Kernel.Prelude.Double, userLon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlacesResponse = PlacesResponse {popularLocations :: [MultiModalLocation], recentLocations :: [MultiModalLocation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
