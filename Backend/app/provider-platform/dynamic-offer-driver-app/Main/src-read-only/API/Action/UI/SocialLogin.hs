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
import qualified Tools.ActorInfo
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
      :<|> "social"
      :> "login"
      :> "v2"
      :> ReqBody
           '[JSON]
           API.Types.UI.SocialLogin.SocialLoginV2Req
      :> Post
           '[JSON]
           API.Types.UI.SocialLogin.SocialLoginV2Res
      :<|> "auth"
      :> "email"
      :> "v2"
      :> Header
           "x-forwarded-for"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.SocialLogin.EmailAuthV2Req
      :> Post
           '[JSON]
           API.Types.UI.SocialLogin.EmailAuthV2Res
      :<|> "auth"
      :> "email"
      :> Capture
           "authId"
           Kernel.Prelude.Text
      :> "verify"
      :> "v2"
      :> ReqBody
           '[JSON]
           API.Types.UI.SocialLogin.EmailAuthVerifyV2Req
      :> Post
           '[JSON]
           API.Types.UI.SocialLogin.SocialLoginV2Res
      :<|> TokenAuth
      :> "social"
      :> "link"
      :> ReqBody
           '[JSON]
           API.Types.UI.SocialLogin.SocialLinkReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "social"
      :> "unlink"
      :> ReqBody
           '[JSON]
           API.Types.UI.SocialLogin.SocialUnlinkReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "social"
      :> "linkedAccounts"
      :> Get
           '[JSON]
           API.Types.UI.SocialLogin.LinkedSocialAccountsRes
  )

handler :: Environment.FlowServer API
handler = postSocialLogin :<|> postSocialUpdateProfile :<|> postSocialLoginV2 :<|> postAuthEmailV2 :<|> postAuthEmailVerifyV2 :<|> postSocialLink :<|> postSocialUnlink :<|> getSocialLinkedAccounts

postSocialLogin :: (API.Types.UI.SocialLogin.SocialLoginReq -> Environment.FlowHandler API.Types.UI.SocialLogin.SocialLoginRes)
postSocialLogin a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SocialLogin.postSocialLogin a1

postSocialUpdateProfile ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SocialLogin.SocialUpdateProfileReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSocialUpdateProfile a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SocialLogin.postSocialUpdateProfile (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSocialLoginV2 :: (API.Types.UI.SocialLogin.SocialLoginV2Req -> Environment.FlowHandler API.Types.UI.SocialLogin.SocialLoginV2Res)
postSocialLoginV2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SocialLogin.postSocialLoginV2 a1

postAuthEmailV2 :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.UI.SocialLogin.EmailAuthV2Req -> Environment.FlowHandler API.Types.UI.SocialLogin.EmailAuthV2Res)
postAuthEmailV2 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SocialLogin.postAuthEmailV2 a2 a1

postAuthEmailVerifyV2 :: (Kernel.Prelude.Text -> API.Types.UI.SocialLogin.EmailAuthVerifyV2Req -> Environment.FlowHandler API.Types.UI.SocialLogin.SocialLoginV2Res)
postAuthEmailVerifyV2 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.SocialLogin.postAuthEmailVerifyV2 a2 a1

postSocialLink ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SocialLogin.SocialLinkReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSocialLink a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SocialLogin.postSocialLink (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSocialUnlink ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SocialLogin.SocialUnlinkReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSocialUnlink a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SocialLogin.postSocialUnlink (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getSocialLinkedAccounts ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.SocialLogin.LinkedSocialAccountsRes
  )
getSocialLinkedAccounts a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.SocialLogin.getSocialLinkedAccounts (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
