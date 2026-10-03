{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Dispatcher where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data DepotVehicle = DepotVehicle {fleet_no :: Data.Text.Text, status :: Kernel.Prelude.Maybe Data.Text.Text, vehicle_no :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DispatcherHistoryRes = DispatcherHistoryRes
  { createdAt :: Kernel.Prelude.UTCTime,
    currentVehicle :: Data.Text.Text,
    depotId :: Data.Text.Text,
    dispatcherId :: Data.Text.Text,
    historyConductorCode :: Data.Text.Text,
    historyDriverCode :: Data.Text.Text,
    id :: Data.Text.Text,
    reasonContent :: Kernel.Prelude.Maybe Data.Text.Text,
    reasonTag :: Data.Text.Text,
    replacedVehicle :: Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    waybillNo :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DispatcherReq = DispatcherReq {reason :: ScheduleUpdateReasons, sourceFleetId :: Data.Text.Text, updatedFleetId :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DispatcherRes = DispatcherRes {conductorCode :: Data.Text.Text, depotName :: Data.Text.Text, driverCode :: Data.Text.Text, scheduleNo :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduleUpdateReasons
  = BreakDown
  | OtherReason Data.Text.Text
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UserDepotRes = UserDepotRes {depot :: Kernel.Prelude.Maybe Data.Text.Text, depotName :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
