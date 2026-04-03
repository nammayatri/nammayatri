{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.PriceBreakup 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.PriceBreakup
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.Ride
import qualified API.Types.UI.PriceBreakup
import qualified API.Types.UI.DriverOnboardingV2
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "meterRide" :> "price" :> MandatoryQueryParam "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> ReqBody ('[JSON])
                                                                                                                                       API.Types.UI.PriceBreakup.MeterRidePriceReq :> Post ('[JSON])
                                                                                                                                                                                           API.Types.UI.PriceBreakup.MeterRidePriceRes :<|> TokenAuth :> "priceBreakup" :> MandatoryQueryParam "rideId"
                                                                                                                                                                                                                                                                                               (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                                                  [API.Types.UI.DriverOnboardingV2.RateCardItem])
handler :: Environment.FlowServer API
handler = postMeterRidePrice :<|> getPriceBreakup
postMeterRidePrice :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                        Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                        Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> API.Types.UI.PriceBreakup.MeterRidePriceReq -> Environment.FlowHandler API.Types.UI.PriceBreakup.MeterRidePriceRes)
postMeterRidePrice a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PriceBreakup.postMeterRidePrice (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
getPriceBreakup :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                     Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler [API.Types.UI.DriverOnboardingV2.RateCardItem])
getPriceBreakup a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PriceBreakup.getPriceBreakup (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



