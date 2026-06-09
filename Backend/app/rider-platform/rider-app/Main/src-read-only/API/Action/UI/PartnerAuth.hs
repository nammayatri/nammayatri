{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PartnerAuth
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PartnerAuth
import qualified Domain.Action.UI.PartnerAuth
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "partner" :> Capture "provider" Kernel.Prelude.Text :> "session" :> Header "x-forwarded-for" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.PartnerAuth.PartnerSessionReq
      :> Post ('[JSON]) API.Types.UI.PartnerAuth.PartnerSessionRes
  )

handler :: Environment.FlowServer API
handler = postPartnerSession

postPartnerSession :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.PartnerAuth.PartnerSessionReq -> Environment.FlowHandler API.Types.UI.PartnerAuth.PartnerSessionRes)
postPartnerSession a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PartnerAuth.postPartnerSession a3 a2 a1
