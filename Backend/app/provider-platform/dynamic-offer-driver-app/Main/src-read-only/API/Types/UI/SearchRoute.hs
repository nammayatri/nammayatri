{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SearchRoute where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.RideRoute
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps
import Servant
import Tools.Auth

data SearchRouteResp = SearchRouteResp {pickupRoute :: Kernel.External.Maps.GetRoutesResp, tripRoute :: Domain.Types.RideRoute.RouteInfo}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
