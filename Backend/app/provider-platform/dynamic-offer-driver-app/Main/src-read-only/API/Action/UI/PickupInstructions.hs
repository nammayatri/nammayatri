{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PickupInstructions
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PickupInstructions
import qualified Control.Lens
import qualified Domain.Action.UI.PickupInstructions as Domain.Action.UI.PickupInstructions
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "driver" :> "pickupInstructions" :> MandatoryQueryParam "riderId" Kernel.Prelude.Text :> Get ('[JSON]) API.Types.UI.PickupInstructions.PickupInstructionResp)

handler :: Environment.FlowServer API
handler = getDriverPickupInstructions

getDriverPickupInstructions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.PickupInstructions.PickupInstructionResp
  )
getDriverPickupInstructions a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.getDriverPickupInstructions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
