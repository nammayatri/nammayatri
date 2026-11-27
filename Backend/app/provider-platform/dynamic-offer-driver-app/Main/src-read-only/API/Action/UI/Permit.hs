{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Permit
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Permit
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Permit.Api.Types
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Lib.Permit.Types
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( "permit" :> "auth" :> "otp" :> Header "x-forwarded-for" Kernel.Prelude.Text :> ReqBody '[JSON] Lib.Permit.Api.Types.PermitAuthOtpReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "permit"
      :> "auth"
      :> "verify"
      :> ReqBody
           '[JSON]
           Lib.Permit.Api.Types.PermitAuthVerifyReq
      :> Post
           '[JSON]
           Lib.Permit.Api.Types.PermitAuthVerifyRes
      :<|> "permit"
      :> "auth"
      :> "logout"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "permit"
      :> "application"
      :> "list"
      :> MandatoryQueryParam
           "permitType"
           Lib.Permit.Types.PermitType
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           Lib.Permit.Api.Types.PermitApplicationListRes
      :<|> "permit"
      :> "application"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Lib.Permit.Api.Types.PermitCreateApplicationReq
      :> Post
           '[JSON]
           Lib.Permit.Api.Types.PermitCreateApplicationRes
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Lib.Permit.Api.Types.PermitUpdateApplicationReq
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           Lib.Permit.Api.Types.PermitMyApplicationRes
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "document"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Lib.Permit.Api.Types.PermitDocumentUploadReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "undertakingOtp"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> Post
           '[JSON]
           Lib.Permit.Api.Types.PermitUndertakingOtpRes
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "submit"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           Lib.Permit.Api.Types.PermitSubmitReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "withdraw"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "permit"
      :> "application"
      :> Capture
           "applicationId"
           (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication)
      :> "pass"
      :> Header
           "x-permit-token"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           Lib.Permit.Api.Types.PermitPassRes
  )

handler :: Environment.FlowServer API
handler = postPermitAuthOtp :<|> postPermitAuthVerify :<|> postPermitAuthLogout :<|> getPermitApplicationList :<|> postPermitApplication :<|> putPermitApplication :<|> getPermitApplication :<|> postPermitApplicationDocument :<|> postPermitApplicationUndertakingOtp :<|> postPermitApplicationSubmit :<|> postPermitApplicationWithdraw :<|> getPermitApplicationPass

postPermitAuthOtp :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitAuthOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitAuthOtp a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitAuthOtp a2 a1

postPermitAuthVerify :: (Lib.Permit.Api.Types.PermitAuthVerifyReq -> Environment.FlowHandler Lib.Permit.Api.Types.PermitAuthVerifyRes)
postPermitAuthVerify a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitAuthVerify a1

postPermitAuthLogout :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitAuthLogout a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitAuthLogout a1

getPermitApplicationList :: (Lib.Permit.Types.PermitType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Lib.Permit.Api.Types.PermitApplicationListRes)
getPermitApplicationList a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.getPermitApplicationList a2 a1

postPermitApplication :: (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitCreateApplicationReq -> Environment.FlowHandler Lib.Permit.Api.Types.PermitCreateApplicationRes)
postPermitApplication a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitApplication a2 a1

putPermitApplication :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitUpdateApplicationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putPermitApplication a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.putPermitApplication a3 a2 a1

getPermitApplication :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Lib.Permit.Api.Types.PermitMyApplicationRes)
getPermitApplication a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.getPermitApplication a2 a1

postPermitApplicationDocument :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitDocumentUploadReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitApplicationDocument a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitApplicationDocument a3 a2 a1

postPermitApplicationUndertakingOtp :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Lib.Permit.Api.Types.PermitUndertakingOtpRes)
postPermitApplicationUndertakingOtp a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitApplicationUndertakingOtp a2 a1

postPermitApplicationSubmit :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Lib.Permit.Api.Types.PermitSubmitReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitApplicationSubmit a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitApplicationSubmit a3 a2 a1

postPermitApplicationWithdraw :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPermitApplicationWithdraw a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.postPermitApplicationWithdraw a2 a1

getPermitApplicationPass :: (Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Lib.Permit.Api.Types.PermitPassRes)
getPermitApplicationPass a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withRequestIdActorInfo $ Domain.Action.UI.Permit.getPermitApplicationPass a2 a1
