{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.SocialLogin 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.SocialLogin
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.SocialLogin
import qualified Kernel.Types.APISuccess



type API = ("social" :> "login" :> ReqBody ('[JSON]) API.Types.UI.SocialLogin.SocialLoginReq :> Post ('[JSON])
                                                                                                     API.Types.UI.SocialLogin.SocialLoginRes :<|> TokenAuth :> "social" :> "update" :> "profile" :> ReqBody ('[JSON]) API.Types.UI.SocialLogin.SocialUpdateProfileReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                              Kernel.Types.APISuccess.APISuccess)
handler :: Environment.FlowServer API
handler = postSocialLogin :<|> postSocialUpdateProfile
postSocialLogin :: (API.Types.UI.SocialLogin.SocialLoginReq -> Environment.FlowHandler API.Types.UI.SocialLogin.SocialLoginRes)
postSocialLogin a1 = withFlowHandlerAPI $ Domain.Action.UI.SocialLogin.postSocialLogin a1
postSocialUpdateProfile :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                             Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.SocialLogin.SocialUpdateProfileReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSocialUpdateProfile a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SocialLogin.postSocialUpdateProfile (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



