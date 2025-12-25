module Domain.Action.ProviderPlatform.RideBooking.MeterRide (getMeterRidePrice) where

import qualified API.Client.ProviderPlatform.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.UI.PriceBreakup
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Ride
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getMeterRidePrice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.Flow API.Types.UI.PriceBreakup.MeterRidePriceRes)
getMeterRidePrice merchantShortId opCity rideId = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.ProviderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.meterRideDSL.getMeterRidePrice) rideId
