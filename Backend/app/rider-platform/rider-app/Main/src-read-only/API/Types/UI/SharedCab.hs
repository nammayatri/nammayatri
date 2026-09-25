{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCab where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data SharedCabLiveCab = SharedCabLiveCab {freeSeats :: Kernel.Prelude.Int, plateLast4 :: Kernel.Prelude.Text, position :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteDetailResp = SharedCabRouteDetailResp {cabs :: [SharedCabLiveCab], routeInfo :: SharedCabRouteInfo}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteInfo = SharedCabRouteInfo {cabsRunning :: Kernel.Prelude.Int, name :: Kernel.Prelude.Text, polyline :: Kernel.Prelude.Maybe Kernel.Prelude.Text, routeCode :: Kernel.Prelude.Text, stops :: [SharedCabStop]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabRouteListResp = SharedCabRouteListResp {routes :: [SharedCabRouteInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabStop = SharedCabStop {code :: Kernel.Prelude.Text, name :: Kernel.Prelude.Text, point :: Kernel.External.Maps.Types.LatLong, sequenceNum :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
