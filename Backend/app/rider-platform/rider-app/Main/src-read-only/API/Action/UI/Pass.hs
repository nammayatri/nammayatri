{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Pass
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Pass
import qualified Control.Lens
import qualified Domain.Action.UI.Pass
import qualified Domain.Types.Merchant
import qualified Domain.Types.Pass
import qualified Domain.Types.Person
import qualified Domain.Types.PurchasedPass
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "multimodal" :> "pass" :> "availablePasses" :> Get ('[JSON]) [API.Types.UI.Pass.PassInfoAPIEntity] :<|> TokenAuth :> "multimodal" :> "pass"
      :> Capture
           "passId"
           (Kernel.Types.Id.Id Domain.Types.Pass.Pass)
      :> "select"
      :> Post
           ('[JSON])
           API.Types.UI.Pass.PassSelectionAPIEntity
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> Capture
           "purchasedPassId"
           (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass)
      :> "status"
      :> Get
           ('[JSON])
           API.Types.UI.Pass.PurchasedPassAPIEntity
      :<|> TokenAuth
      :> "multimodal"
      :> "pass"
      :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.PurchasedPass.StatusType
      :> Get
           ('[JSON])
           [API.Types.UI.Pass.PurchasedPassAPIEntity]
  )

handler :: Environment.FlowServer API
handler = getMultimodalPassAvailablePasses :<|> postMultimodalPassSelect :<|> getMultimodalPassStatus :<|> getMultimodalPassList

getMultimodalPassAvailablePasses ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.Pass.PassInfoAPIEntity]
  )
getMultimodalPassAvailablePasses a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.getMultimodalPassAvailablePasses (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postMultimodalPassSelect ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Pass.Pass ->
    Environment.FlowHandler API.Types.UI.Pass.PassSelectionAPIEntity
  )
postMultimodalPassSelect a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.postMultimodalPassSelect (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getMultimodalPassStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass ->
    Environment.FlowHandler API.Types.UI.Pass.PurchasedPassAPIEntity
  )
getMultimodalPassStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.getMultimodalPassStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getMultimodalPassList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Domain.Types.PurchasedPass.StatusType) ->
    Environment.FlowHandler [API.Types.UI.Pass.PurchasedPassAPIEntity]
  )
getMultimodalPassList a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Pass.getMultimodalPassList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
