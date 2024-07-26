{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard as Domain.Action.Dashboard
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (ApiTokenAuth :> "tag" :> "create" :> ReqBody ('[JSON]) Lib.Yudhishthira.Types.NammaTag :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = postTagCreate

postTagCreate :: (Verified -> Lib.Yudhishthira.Types.NammaTag -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postTagCreate a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.postTagCreate a2 a1
