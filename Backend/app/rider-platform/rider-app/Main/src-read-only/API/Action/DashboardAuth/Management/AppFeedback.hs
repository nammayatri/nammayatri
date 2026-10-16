{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.AppFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.AppFeedback
import qualified Domain.Action.Dashboard.AppFeedback
import qualified Domain.Types.AppFeedbackConfig
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("appFeedback" :> (GetAppFeedbackMeta :<|> GetAppFeedbackInspectPreview :<|> GetAppFeedbackInspectResponses :<|> PostAppFeedbackResponseRetryActions))

type GetAppFeedbackMeta = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/APP_FEEDBACK/GET_APP_FEEDBACK_META" :> API.Types.RiderPlatform.Management.AppFeedback.GetAppFeedbackMeta)

type GetAppFeedbackInspectPreview =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/APP_FEEDBACK/GET_APP_FEEDBACK_INSPECT_PREVIEW"
      :> API.Types.RiderPlatform.Management.AppFeedback.GetAppFeedbackInspectPreview
  )

type GetAppFeedbackInspectResponses =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/APP_FEEDBACK/GET_APP_FEEDBACK_INSPECT_RESPONSES"
      :> API.Types.RiderPlatform.Management.AppFeedback.GetAppFeedbackInspectResponses
  )

type PostAppFeedbackResponseRetryActions =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/APP_FEEDBACK/POST_APP_FEEDBACK_RESPONSE_RETRY_ACTIONS"
      :> API.Types.RiderPlatform.Management.AppFeedback.PostAppFeedbackResponseRetryActions
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getAppFeedbackMeta merchantId city :<|> getAppFeedbackInspectPreview merchantId city :<|> getAppFeedbackInspectResponses merchantId city :<|> postAppFeedbackResponseRetryActions merchantId city

getAppFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.RiderPlatform.Management.AppFeedback.AppFeedbackMetaRes)
getAppFeedbackMeta a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.AppFeedback.getAppFeedbackMeta a3 a2

getAppFeedbackInspectPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Domain.Types.AppFeedbackConfig.AppFeedbackPlacement -> Environment.FlowHandler API.Types.RiderPlatform.Management.AppFeedback.AppFeedbackPreviewRes)
getAppFeedbackInspectPreview a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppFeedback.getAppFeedbackInspectPreview a6 a5 a3 a2 a1

getAppFeedbackInspectResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.RiderPlatform.Management.AppFeedback.AppFeedbackResponseRes])
getAppFeedbackInspectResponses a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppFeedback.getAppFeedbackInspectResponses a4 a3 a1

postAppFeedbackResponseRetryActions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAppFeedbackResponseRetryActions a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/APP_FEEDBACK/POST_APP_FEEDBACK_RESPONSE_RETRY_ACTIONS" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppFeedback.postAppFeedbackResponseRetryActions a4 a3 a1
    )
