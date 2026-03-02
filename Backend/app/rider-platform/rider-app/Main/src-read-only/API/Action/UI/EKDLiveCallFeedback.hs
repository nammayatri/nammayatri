{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.EKDLiveCallFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.UI.EKDLiveCallFeedback
import qualified Domain.Action.UI.EKDLiveCallFeedback
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = ("ekdLiveCallFeedback" :> ReqBody ('[JSON]) API.Types.UI.EKDLiveCallFeedback.EKDLiveCallFeedbackReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

handler :: Environment.FlowServer API
handler = postEkdLiveCallFeedback

postEkdLiveCallFeedback :: (API.Types.UI.EKDLiveCallFeedback.EKDLiveCallFeedbackReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEkdLiveCallFeedback a1 = withFlowHandlerAPI $ Domain.Action.UI.EKDLiveCallFeedback.postEkdLiveCallFeedback a1
