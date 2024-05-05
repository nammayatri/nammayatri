{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SocialLogin where

import qualified API.Types.UI.SocialLogin
import qualified Domain.Action.UI.SocialLogin as Domain.Action.UI.SocialLogin
import qualified Environment
import EulerHS.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = ("social" :> "login" :> ReqBody '[JSON] API.Types.UI.SocialLogin.SocialLoginReq :> Post '[JSON] API.Types.UI.SocialLogin.SocialLoginRes)

handler :: Environment.FlowServer API
handler = postSocialLogin

postSocialLogin :: (API.Types.UI.SocialLogin.SocialLoginReq -> Environment.FlowHandler API.Types.UI.SocialLogin.SocialLoginRes)
postSocialLogin a1 = withFlowHandlerAPI $ Domain.Action.UI.SocialLogin.postSocialLogin a1
