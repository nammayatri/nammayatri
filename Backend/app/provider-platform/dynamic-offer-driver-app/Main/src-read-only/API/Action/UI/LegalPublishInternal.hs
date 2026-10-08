{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.LegalPublishInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.LegalPublishInternal
import qualified Domain.Action.UI.LegalPublishInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( "internal" :> "legal" :> "publish" :> Header "x-internal-api-key" Kernel.Prelude.Text :> ReqBody ('[JSON]) API.Types.UI.LegalPublishInternal.LegalPublishReq
      :> Post
           ('[JSON])
           API.Types.UI.LegalPublishInternal.LegalPublishResp
  )

handler :: Environment.FlowServer API
handler = postInternalLegalPublish

postInternalLegalPublish :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.LegalPublishInternal.LegalPublishReq -> Environment.FlowHandler API.Types.UI.LegalPublishInternal.LegalPublishResp)
postInternalLegalPublish a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.LegalPublishInternal.postInternalLegalPublish a2 a1
