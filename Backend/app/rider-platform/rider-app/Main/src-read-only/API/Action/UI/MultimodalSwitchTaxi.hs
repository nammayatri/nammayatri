{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MultimodalSwitchTaxi
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.MultimodalSwitchTaxi as Domain.Action.UI.MultimodalSwitchTaxi
import qualified Domain.Types.Estimate
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
  ( TokenAuth :> "multimodal" :> Capture "searchRequestId" (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest) :> "switch"
      :> Capture
           "estimateId"
           (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate)
      :> "taxi"
      :> Get ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getMultimodalSwitchTaxi

getMultimodalSwitchTaxi ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest ->
    Kernel.Types.Id.Id Domain.Types.Estimate.Estimate ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
getMultimodalSwitchTaxi a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalSwitchTaxi.getMultimodalSwitchTaxi (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
