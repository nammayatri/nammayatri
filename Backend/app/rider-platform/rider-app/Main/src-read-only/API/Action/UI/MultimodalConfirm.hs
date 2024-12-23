{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MultimodalConfirm
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MultimodalConfirm
import qualified Control.Lens
import qualified Domain.Action.UI.MultimodalConfirm as Domain.Action.UI.MultimodalConfirm
import qualified Domain.Types.Estimate
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
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
  ( TokenAuth :> "multimodal" :> Capture "journeyId" (Kernel.Types.Id.Id Domain.Types.Journey.Journey) :> "info"
      :> ReqBody
           ('[JSON])
           API.Types.UI.MultimodalConfirm.JourneyInfoReq
      :> Post ('[JSON]) API.Types.UI.MultimodalConfirm.JourneyInfoResp
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "confirm"
      :> ReqBody
           ('[JSON])
           API.Types.UI.MultimodalConfirm.JourneyConfirmReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "searchRequestId"
           (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest)
      :> "switch"
      :> Capture
           "estimateId"
           (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate)
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "legId"
           Kernel.Prelude.Text
      :> "switchToAuto"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postMultimodalInfo :<|> postMultimodalConfirm :<|> postMultimodalSwitch :<|> postMultimodalSwitchToAuto

postMultimodalInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.JourneyInfoReq ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalInfo a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.JourneyConfirmReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalConfirm a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalSwitch ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest ->
    Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalSwitch a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalSwitch (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalSwitchToAuto ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalSwitchToAuto a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalSwitchToAuto (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
