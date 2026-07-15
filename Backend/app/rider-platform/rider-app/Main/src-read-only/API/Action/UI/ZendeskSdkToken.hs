{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ZendeskSdkToken
  ( API,
    handler,
  )
where

import qualified API.Types.UI.ZendeskSdkToken
import qualified Domain.Action.UI.ZendeskSdkToken
import qualified Environment
import EulerHS.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = ("profile" :> "zendeskSdkToken" :> ReqBody ('[JSON]) API.Types.UI.ZendeskSdkToken.ZendeskJwtReq :> Post ('[JSON]) API.Types.UI.ZendeskSdkToken.ZendeskJwtResp)

handler :: Environment.FlowServer API
handler = postProfileZendeskSdkToken

postProfileZendeskSdkToken :: (API.Types.UI.ZendeskSdkToken.ZendeskJwtReq -> Environment.FlowHandler API.Types.UI.ZendeskSdkToken.ZendeskJwtResp)
postProfileZendeskSdkToken a1 = withFlowHandlerAPI $ Domain.Action.UI.ZendeskSdkToken.postProfileZendeskSdkToken a1
