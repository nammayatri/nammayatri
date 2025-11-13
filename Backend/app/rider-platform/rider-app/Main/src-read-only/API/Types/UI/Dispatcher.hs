{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Dispatcher where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DepotVehicle = DepotVehicle {fleet_no :: Kernel.Prelude.Text, status :: Kernel.Prelude.Text, vehicle_no :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DispatcherReq = DispatcherReq {reason :: ScheduleUpdateReasons, sourceFleetId :: Kernel.Prelude.Text, updatedFleetId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DispatcherRes = DispatcherRes {conductorCode :: Kernel.Prelude.Text, depotName :: Kernel.Prelude.Text, driverCode :: Kernel.Prelude.Text, scheduleNo :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduleUpdateReasons
  = BreakDown
  | OtherReason Kernel.Prelude.Text
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
