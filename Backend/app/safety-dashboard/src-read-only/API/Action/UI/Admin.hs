{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Admin
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Admin
import qualified API.Types.UI.Suspect
import qualified Domain.Action.UI.Admin
import qualified Domain.Types.Suspect
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth

type API =
  ( DashboardAuth 'DASHBOARD_ADMIN :> "change" :> "suspectFlag" :> ReqBody '[JSON] API.Types.UI.Admin.SuspectFlagChangeRequestList
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'DASHBOARD_ADMIN
      :> "admin"
      :> "upload"
      :> "suspect"
      :> "bulk"
      :> QueryParam
           "flaggedStatus"
           Domain.Types.Suspect.FlaggedStatus
      :> ReqBody
           '[JSON]
           API.Types.UI.Suspect.SuspectBulkUploadReq
      :> Post
           '[JSON]
           API.Types.UI.Suspect.SuspectBulkUploadResp
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "admin"
      :> "upload"
      :> "suspect"
      :> "bulk"
      :> ReqBody
           '[JSON]
           API.Types.UI.Suspect.SuspectBulkUploadReq
      :> Post
           '[JSON]
           API.Types.UI.Suspect.SuspectBulkUploadResp
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "check"
      :> "webhook"
      :> ReqBody
           '[JSON]
           API.Types.UI.Admin.WebhookCheck
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "user"
      :> "assignRole"
      :> ReqBody
           '[JSON]
           API.Types.UI.Admin.AssignRoleMerchantUserReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "user"
      :> "delete"
      :> ReqBody
           '[JSON]
           API.Types.UI.Admin.DeleteMerchantUserReq
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postChangeSuspectFlag :<|> postAdminUploadSuspectBulk :<|> postMerchantAdminUploadSuspectBulk :<|> postCheckWebhook :<|> postMerchantUserAssignRole :<|> deleteMerchantUserDelete

postChangeSuspectFlag :: (TokenInfo -> API.Types.UI.Admin.SuspectFlagChangeRequestList -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postChangeSuspectFlag a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Admin.postChangeSuspectFlag a2 a1

postAdminUploadSuspectBulk :: (TokenInfo -> Kernel.Prelude.Maybe Domain.Types.Suspect.FlaggedStatus -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.FlowHandler API.Types.UI.Suspect.SuspectBulkUploadResp)
postAdminUploadSuspectBulk a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Admin.postAdminUploadSuspectBulk a3 a2 a1

postMerchantAdminUploadSuspectBulk :: (TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.FlowHandler API.Types.UI.Suspect.SuspectBulkUploadResp)
postMerchantAdminUploadSuspectBulk a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Admin.postMerchantAdminUploadSuspectBulk a2 a1

postCheckWebhook :: (TokenInfo -> API.Types.UI.Admin.WebhookCheck -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCheckWebhook a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Admin.postCheckWebhook a2 a1

postMerchantUserAssignRole :: (TokenInfo -> API.Types.UI.Admin.AssignRoleMerchantUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUserAssignRole a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Admin.postMerchantUserAssignRole a2 a1

deleteMerchantUserDelete :: (TokenInfo -> API.Types.UI.Admin.DeleteMerchantUserReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteMerchantUserDelete a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Admin.deleteMerchantUserDelete a2 a1
