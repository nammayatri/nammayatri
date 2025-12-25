{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SocialLogin
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SocialLogin
import qualified Control.Lens
import qualified Domain.Action.UI.SocialLogin
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
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
  ( "social" :> "login" :> ReqBody '[JSON] API.Types.UI.SocialLogin.SocialLoginReq
      :> Post
           '[JSON]
           API.Types.UI.SocialLogin.SocialLoginRes
      :<|> TokenAuth
      :> "social"
      :> "update"
      :> "profile"
      :> ReqBody '[JSON] API.Types.UI.SocialLogin.SocialUpdateProfileReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postSocialLogin :<|> postSocialUpdateProfile

postSocialLogin :: (API.Types.UI.SocialLogin.SocialLoginReq -> Environment.FlowHandler API.Types.UI.SocialLogin.SocialLoginRes)
postSocialLogin a1 = withFlowHandlerAPI $ Domain.Action.UI.SocialLogin.postSocialLogin a1

postSocialUpdateProfile ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SocialLogin.SocialUpdateProfileReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSocialUpdateProfile a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SocialLogin.postSocialUpdateProfile (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
