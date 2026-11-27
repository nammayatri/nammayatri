{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handler for the direct-dashboard list of a driver's rides: the driver the request names has to be one a fleet owner's scope holds.
module Domain.Action.DashboardAuth.AppManagement.Driver
  ( getDriverFleetListRides,
  )
where

import qualified Data.Time.Calendar
import qualified Domain.Action.Dashboard.AppManagement.Driver
import qualified Domain.Action.UI.Ride
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FleetScope
import Tools.Auth
import Tools.Auth.DashboardUserAuth

getDriverFleetListRides :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow Domain.Action.UI.Ride.DriverRideListRes)
getDriverFleetListRides a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a10 a9
  Domain.Action.Dashboard.AppManagement.Driver.getDriverFleetListRides a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1
