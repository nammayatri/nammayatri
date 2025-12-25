module Domain.Action.Dashboard.AppManagement.Driver (getDriverFleetListRides) where

import Data.Maybe
import qualified Data.Time.Calendar
import qualified "this" Domain.Action.UI.Ride
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Ride
import qualified Environment
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id

getDriverFleetListRides ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus ->
  Kernel.Prelude.Maybe Data.Time.Calendar.Day ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Environment.Flow Domain.Action.UI.Ride.DriverRideListRes
getDriverFleetListRides _merchantShortId _opCity driverId mbLimit mbOffset mbOnlyActive mbStatus mbDay mbFleetOwnerId mbNumOfDays =
  Domain.Action.UI.Ride.listDriverRides driverId Nothing mbLimit mbOffset mbOnlyActive mbStatus mbDay mbFleetOwnerId mbNumOfDays
