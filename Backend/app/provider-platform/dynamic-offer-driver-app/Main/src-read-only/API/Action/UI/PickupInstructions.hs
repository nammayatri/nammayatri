{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.PickupInstructions 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.PickupInstructions
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Domain.Types.Ride
import qualified API.Types.UI.PickupInstructions
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "pickupInstructions" :> MandatoryQueryParam "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> Get ('[JSON]) API.Types.UI.PickupInstructions.PickupInstructionResp)
handler :: Environment.FlowServer API
handler = getPickupInstructions
getPickupInstructions :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.PickupInstructions.PickupInstructionResp)
getPickupInstructions a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.getPickupInstructions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



