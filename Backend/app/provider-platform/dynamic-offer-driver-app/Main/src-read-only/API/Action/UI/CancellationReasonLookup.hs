{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.CancellationReasonLookup 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.CancellationReasonLookup
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.Ride
import qualified API.Types.UI.CancellationReasonLookup
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "ride" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> "getCancellationReasons" :> Get ('[JSON])
                                                                                                                                   [API.Types.UI.CancellationReasonLookup.CancellationReasonResp])
handler :: Environment.FlowServer API
handler = getRideGetCancellationReasons
getRideGetCancellationReasons :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                                   Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                   Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler [API.Types.UI.CancellationReasonLookup.CancellationReasonResp])
getRideGetCancellationReasons a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.CancellationReasonLookup.getRideGetCancellationReasons (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



