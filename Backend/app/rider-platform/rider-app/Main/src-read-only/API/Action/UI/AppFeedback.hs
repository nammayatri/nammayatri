{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AppFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AppFeedback
import qualified Control.Lens
import qualified Domain.Action.UI.AppFeedback
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "appFeedback" :> "manifest" :> Header "x-language" Kernel.External.Types.Language
      :> Get
           ('[JSON])
           API.Types.UI.AppFeedback.AppFeedbackManifestRes
      :<|> TokenAuth
      :> "appFeedback"
      :> ReqBody ('[JSON]) API.Types.UI.AppFeedback.AppFeedbackSubmitReq
      :> Post
           ('[JSON])
           API.Types.UI.AppFeedback.AppFeedbackSubmitRes
  )

handler :: Environment.FlowServer API
handler = getAppFeedbackManifest :<|> postAppFeedback

getAppFeedbackManifest ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.External.Types.Language) ->
    Environment.FlowHandler API.Types.UI.AppFeedback.AppFeedbackManifestRes
  )
getAppFeedbackManifest a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.AppFeedback.getAppFeedbackManifest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postAppFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.AppFeedback.AppFeedbackSubmitReq ->
    Environment.FlowHandler API.Types.UI.AppFeedback.AppFeedbackSubmitRes
  )
postAppFeedback a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.AppFeedback.postAppFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
