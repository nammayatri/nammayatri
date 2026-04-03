{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.ProviderPlatform.RideBooking.MeterRide 
( API,
handler )
where
import EulerHS.Prelude hiding (sortOn)
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common hiding (INFO)
import Storage.Beam.CommonInstances ()
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.MeterRide
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified Domain.Action.ProviderPlatform.RideBooking.MeterRide
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "dynamic-offer-driver-app" Domain.Types.Ride
import qualified "dynamic-offer-driver-app" API.Types.UI.PriceBreakup
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("meterRide" :> GetMeterRidePrice)
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getMeterRidePrice merchantId city
type GetMeterRidePrice = API.Types.Dashboard.RideBooking.MeterRide.GetMeterRidePrice
getMeterRidePrice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.PriceBreakup.MeterRidePriceRes)
getMeterRidePrice merchantShortId opCity rideId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.MeterRide.getMeterRidePrice merchantShortId opCity rideId



