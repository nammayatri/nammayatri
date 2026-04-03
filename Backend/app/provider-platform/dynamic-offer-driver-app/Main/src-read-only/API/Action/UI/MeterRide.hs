{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.MeterRide 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.MeterRide
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.Ride
import qualified API.Types.UI.MeterRide
import qualified Kernel.Types.APISuccess
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "meterRide" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "addDestination" :> ReqBody ('[JSON])
                                                                                                                                    API.Types.UI.MeterRide.MeterRideAddDestinationReq :> Post ('[JSON]) API.Types.UI.MeterRide.MeterRideAddDestinationResp :<|> TokenAuth :> "meterRide" :> Capture "rideId"
                                                                                                                                                                                                                                                                                                    (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "shareReceipt" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                             API.Types.UI.MeterRide.SendRecietRequest :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                              Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postMeterRideAddDestination :<|> postMeterRideShareReceipt
postMeterRideAddDestination :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                 Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                 Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> API.Types.UI.MeterRide.MeterRideAddDestinationReq -> Environment.FlowHandler API.Types.UI.MeterRide.MeterRideAddDestinationResp)
postMeterRideAddDestination a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MeterRide.postMeterRideAddDestination (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
postMeterRideShareReceipt :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                               Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                               Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> API.Types.UI.MeterRide.SendRecietRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMeterRideShareReceipt a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MeterRide.postMeterRideShareReceipt (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1



