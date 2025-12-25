{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.Driver
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Driver
import qualified Data.Time.Calendar
import qualified Domain.Action.ProviderPlatform.AppManagement.Driver
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified "dynamic-offer-driver-app" Domain.Types.Ride
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> GetDriverFleetListRides)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverFleetListRides merchantId city

type GetDriverFleetListRides =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.DRIVER / 'API.Types.Dashboard.AppManagement.Driver.GET_DRIVER_FLEET_LIST_RIDES)
      :> API.Types.Dashboard.AppManagement.Driver.GetDriverFleetListRides
  )

getDriverFleetListRides :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler Domain.Action.UI.Ride.DriverRideListRes)
getDriverFleetListRides merchantShortId opCity apiTokenInfo driverId limit offset onlyActive status day fleetOwnerId numOfDays = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Driver.getDriverFleetListRides merchantShortId opCity apiTokenInfo driverId limit offset onlyActive status day fleetOwnerId numOfDays
