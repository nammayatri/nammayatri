{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.AppManagement.Endpoints.Driver where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "this" Domain.Types.Person
import qualified Kernel.Prelude
import qualified "this" Domain.Types.Ride
import qualified Data.Time.Calendar
import qualified "this" Domain.Action.UI.Ride
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Data.Aeson



type API = ("driver" :> GetDriverFleetListRides)
type GetDriverFleetListRides = ("fleet" :> "listRides" :> Capture "driverId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> QueryParam "limit" Kernel.Prelude.Integer :> QueryParam "offset"
                                                                                                                                                                                        Kernel.Prelude.Integer :> QueryParam "onlyActive" Kernel.Prelude.Bool :> QueryParam "status"
                                                                                                                                                                                                                                                                            Domain.Types.Ride.RideStatus :> QueryParam "day"
                                                                                                                                                                                                                                                                                                                       Data.Time.Calendar.Day :> QueryParam "fleetOwnerId"
                                                                                                                                                                                                                                                                                                                                                            Kernel.Prelude.Text :> QueryParam "numOfDays"
                                                                                                                                                                                                                                                                                                                                                                                              Kernel.Prelude.Int :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                        Domain.Action.UI.Ride.DriverRideListRes)
newtype DriverAPIs
  = DriverAPIs {getDriverFleetListRides :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.Ride.RideStatus) -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient Domain.Action.UI.Ride.DriverRideListRes)}
mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
                 where getDriverFleetListRides = driverClient
data DriverUserActionType
    = GET_DRIVER_FLEET_LIST_RIDES
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass ToSchema
instance ToJSON DriverUserActionType
    where toJSON (GET_DRIVER_FLEET_LIST_RIDES) = Data.Aeson.String "GET_DRIVER_FLEET_LIST_RIDES"
instance FromJSON DriverUserActionType
    where parseJSON (Data.Aeson.String "GET_DRIVER_FLEET_LIST_RIDES") = pure GET_DRIVER_FLEET_LIST_RIDES
          parseJSON _ = fail "GET_DRIVER_FLEET_LIST_RIDES expected"

$(Data.Singletons.TH.genSingletons [(''DriverUserActionType)])

