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
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "pickupinstructions" :> ReqBody ('[JSON]) API.Types.UI.PickupInstructions.PickupInstructionsReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "pickupinstructions"
      :> Get ('[JSON]) API.Types.UI.PickupInstructions.PickupInstructionsResp
  )

handler :: Environment.FlowServer API
handler = postPickupinstructions :<|> getPickupinstructions

postPickupinstructions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.PickupInstructions.PickupInstructionsReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPickupinstructions a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.postPickupinstructions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getPickupinstructions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.PickupInstructions.PickupInstructionsResp
  )
getPickupinstructions a1 = withFlowHandlerAPI $ Domain.Action.UI.PickupInstructions.getPickupinstructions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
