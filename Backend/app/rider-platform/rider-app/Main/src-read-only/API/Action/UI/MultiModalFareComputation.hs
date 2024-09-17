{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MultiModalFareComputation
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MultiModalFareComputation
import qualified Control.Lens
import qualified Domain.Action.UI.MultiModalFareComputation as Domain.Action.UI.MultiModalFareComputation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MultiModalFareLegRules
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "multimodal" :> Capture "legRuleId" (Kernel.Types.Id.Id Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules) :> "fare"
      :> Get
           ('[JSON])
           API.Types.UI.MultiModalFareComputation.MultiModalFare
      :<|> TokenAuth
      :> "multimodal"
      :> "fare"
      :> ReqBody
           ('[JSON])
           API.Types.UI.MultiModalFareComputation.GetFareReq
      :> Post
           ('[JSON])
           API.Types.UI.MultiModalFareComputation.MultiModalFare
  )

handler :: Environment.FlowServer API
handler = getMultimodalFare :<|> postMultimodalFare

getMultimodalFare ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules ->
    Environment.FlowHandler API.Types.UI.MultiModalFareComputation.MultiModalFare
  )
getMultimodalFare a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultiModalFareComputation.getMultimodalFare (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postMultimodalFare ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultiModalFareComputation.GetFareReq ->
    Environment.FlowHandler API.Types.UI.MultiModalFareComputation.MultiModalFare
  )
postMultimodalFare a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultiModalFareComputation.postMultimodalFare (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
