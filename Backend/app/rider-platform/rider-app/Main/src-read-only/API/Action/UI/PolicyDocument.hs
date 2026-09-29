{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PolicyDocument
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PolicyDocument
import qualified Control.Lens
import qualified Domain.Action.UI.PolicyDocument
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
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "policy" :> "latest" :> Get ('[JSON]) API.Types.UI.PolicyDocument.PolicyLatestResp :<|> TokenAuth :> "policy" :> "list"
      :> Get
           ('[JSON])
           API.Types.UI.PolicyDocument.PolicyListResp
      :<|> TokenAuth
      :> "policy"
      :> "accept"
      :> ReqBody
           ('[JSON])
           API.Types.UI.PolicyDocument.PolicyAcceptReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getPolicyLatest :<|> getPolicyList :<|> postPolicyAccept

getPolicyLatest :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.PolicyDocument.PolicyLatestResp)
getPolicyLatest a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.PolicyDocument.getPolicyLatest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getPolicyList :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.PolicyDocument.PolicyListResp)
getPolicyList a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.PolicyDocument.getPolicyList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPolicyAccept ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.PolicyDocument.PolicyAcceptReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPolicyAccept a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.PolicyDocument.postPolicyAccept (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
