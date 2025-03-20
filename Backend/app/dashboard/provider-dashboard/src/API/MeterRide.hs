module API.MeterRide
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.UI.DriverOnboardingV2
import qualified "dynamic-offer-driver-app" API.Types.UI.PriceBreakup
import qualified Domain.Action.ProviderPlatform.MeterRide
import qualified "dynamic-offer-driver-app" Domain.Types.Ride
import "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  ( "meterRide"
      :> ( "price" :> MandatoryQueryParam "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
             :> Get
                  ('[JSON])
                  API.Types.UI.PriceBreakup.MeterRidePriceRes
             :<|> "priceBreakup"
             :> MandatoryQueryParam "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
             :> Get
                  ('[JSON])
                  [API.Types.UI.DriverOnboardingV2.RateCardItem]
         )
  )

handler :: FlowServer API
handler = getMeterRidePrice :<|> getPriceBreakup

getMeterRidePrice :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> FlowHandler API.Types.UI.PriceBreakup.MeterRidePriceRes
getMeterRidePrice a1 = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.MeterRide.getMeterRidePrice a1

getPriceBreakup :: Kernel.Types.Id.Id Domain.Types.Ride.Ride -> FlowHandler [API.Types.UI.DriverOnboardingV2.RateCardItem]
getPriceBreakup a1 = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.MeterRide.getPriceBreakup a1
