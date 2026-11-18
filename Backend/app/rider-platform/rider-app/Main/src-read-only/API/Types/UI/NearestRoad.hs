{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.NearestRoad where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data NearestRoadReq = NearestRoadReq {location :: Kernel.External.Maps.Types.LatLong, number :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearestRoadResp = NearestRoadResp {waypoints :: [NearestRoadWaypoint]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NearestRoadWaypoint = NearestRoadWaypoint {distance :: Kernel.Prelude.Double, location :: Kernel.External.Maps.Types.LatLong, name :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
