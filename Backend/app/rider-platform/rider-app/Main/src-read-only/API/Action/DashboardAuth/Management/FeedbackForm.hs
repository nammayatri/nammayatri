{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.FeedbackForm
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.FeedbackForm
import qualified Domain.Action.Dashboard.FeedbackForm
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

type API = ("feedbackForm" :> (GetFeedbackFormList :<|> PostFeedbackFormCreate :<|> PutFeedbackFormUpdate :<|> DeleteFeedbackFormDelete :<|> GetFeedbackForm))

type GetFeedbackFormList = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/FEEDBACK_FORM/GET_FEEDBACK_FORM_LIST" :> API.Types.RiderPlatform.Management.FeedbackForm.GetFeedbackFormList)

type PostFeedbackFormCreate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FEEDBACK_FORM/POST_FEEDBACK_FORM_CREATE"
      :> API.Types.RiderPlatform.Management.FeedbackForm.PostFeedbackFormCreate
  )

type PutFeedbackFormUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FEEDBACK_FORM/PUT_FEEDBACK_FORM_UPDATE"
      :> API.Types.RiderPlatform.Management.FeedbackForm.PutFeedbackFormUpdate
  )

type DeleteFeedbackFormDelete =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/FEEDBACK_FORM/DELETE_FEEDBACK_FORM_DELETE"
      :> API.Types.RiderPlatform.Management.FeedbackForm.DeleteFeedbackFormDelete
  )

type GetFeedbackForm = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/FEEDBACK_FORM/GET_FEEDBACK_FORM" :> API.Types.RiderPlatform.Management.FeedbackForm.GetFeedbackForm)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFeedbackFormList merchantId city :<|> postFeedbackFormCreate merchantId city :<|> putFeedbackFormUpdate merchantId city :<|> deleteFeedbackFormDelete merchantId city :<|> getFeedbackForm merchantId city

getFeedbackFormList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler [API.Types.RiderPlatform.Management.FeedbackForm.FeedbackFormRes])
getFeedbackFormList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.FeedbackForm.getFeedbackFormList a5 a4 a2 a1

postFeedbackFormCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.FeedbackForm.CreateFeedbackFormReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.FeedbackForm.CreateFeedbackFormRes)
postFeedbackFormCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/FEEDBACK_FORM/POST_FEEDBACK_FORM_CREATE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.FeedbackForm.postFeedbackFormCreate a4 a3 a1
    )

putFeedbackFormUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.RiderPlatform.Management.FeedbackForm.UpdateFeedbackFormReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putFeedbackFormUpdate a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/FEEDBACK_FORM/PUT_FEEDBACK_FORM_UPDATE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.FeedbackForm.putFeedbackFormUpdate a5 a4 a2 a1
    )

deleteFeedbackFormDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteFeedbackFormDelete a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/FEEDBACK_FORM/DELETE_FEEDBACK_FORM_DELETE" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.FeedbackForm.deleteFeedbackFormDelete a4 a3 a1
    )

getFeedbackForm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.RiderPlatform.Management.FeedbackForm.FeedbackFormRes)
getFeedbackForm a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.FeedbackForm.getFeedbackForm a4 a3 a1
