{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.ScheduledTrips where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.RouteStopMapping
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data CurrLocation = CurrLocation {lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TrackingResp = TrackingResp {nextStop :: Domain.Types.RouteStopMapping.RouteStopMapping}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
