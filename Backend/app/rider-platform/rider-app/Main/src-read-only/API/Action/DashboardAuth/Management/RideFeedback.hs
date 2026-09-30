{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.RideFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.RideFeedback
import qualified Dashboard.Common
import qualified Data.Text
import qualified Domain.Action.Dashboard.RideFeedback
import qualified Domain.Types.Merchant
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
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

type API = ("rideFeedback" :> (GetRideFeedbackConfigList :<|> GetRideFeedbackConfig :<|> PostRideFeedbackConfigCreate :<|> PostRideFeedbackConfigUpdate :<|> PostRideFeedbackConfigToggle :<|> PostRideFeedbackConfigClone :<|> PostRideFeedbackConfigValidate :<|> GetRideFeedbackRidePreview :<|> GetRideFeedbackRideResponses :<|> PostRideFeedbackResponseRetryActions :<|> GetRideFeedbackMeta))

type GetRideFeedbackConfigList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_CONFIG_LIST"
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackConfigList
  )

type GetRideFeedbackConfig =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_CONFIG"
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackConfig
  )

type PostRideFeedbackConfigCreate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_CREATE"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigCreate
  )

type PostRideFeedbackConfigUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_UPDATE"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigUpdate
  )

type PostRideFeedbackConfigToggle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_TOGGLE"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigToggle
  )

type PostRideFeedbackConfigClone =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_CLONE"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigClone
  )

type PostRideFeedbackConfigValidate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_VALIDATE"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigValidate
  )

type GetRideFeedbackRidePreview =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_PREVIEW"
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackRidePreview
  )

type GetRideFeedbackRideResponses =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_RESPONSES"
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackRideResponses
  )

type PostRideFeedbackResponseRetryActions =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_RESPONSE_RETRY_ACTIONS"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackResponseRetryActions
  )

type GetRideFeedbackMeta = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_META" :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackMeta)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideFeedbackConfigList merchantId city :<|> getRideFeedbackConfig merchantId city :<|> postRideFeedbackConfigCreate merchantId city :<|> postRideFeedbackConfigUpdate merchantId city :<|> postRideFeedbackConfigToggle merchantId city :<|> postRideFeedbackConfigClone merchantId city :<|> postRideFeedbackConfigValidate merchantId city :<|> getRideFeedbackRidePreview merchantId city :<|> getRideFeedbackRideResponses merchantId city :<|> postRideFeedbackResponseRetryActions merchantId city :<|> getRideFeedbackMeta merchantId city

getRideFeedbackConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigListRes)
getRideFeedbackConfigList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackConfigList a7 a6 a4 a3 a2 a1

getRideFeedbackConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigItem)
getRideFeedbackConfig a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackConfig a4 a3 a1

postRideFeedbackConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_CREATE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigCreate a4 a3 a1
    )

postRideFeedbackConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.UpdateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigUpdate a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_UPDATE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigUpdate a5 a4 a2 a1
    )

postRideFeedbackConfigToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.ToggleRideFeedbackConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackConfigToggle a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_TOGGLE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigToggle a5 a4 a2 a1
    )

postRideFeedbackConfigClone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigRes)
postRideFeedbackConfigClone a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_CLONE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigClone a5 a4 a2 a1
    )

postRideFeedbackConfigValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.ValidateRideFeedbackConfigRes)
postRideFeedbackConfigValidate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_CONFIG_VALIDATE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigValidate a4 a3 a1
    )

getRideFeedbackRidePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackPreviewRes)
getRideFeedbackRidePreview a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackRidePreview a4 a3 a1

getRideFeedbackRideResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler [API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackResponseItem])
getRideFeedbackRideResponses a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackRideResponses a4 a3 a1

postRideFeedbackResponseRetryActions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackResponseRetryActions a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_RESPONSE_RETRY_ACTIONS" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackResponseRetryActions a4 a3 a1
    )

getRideFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackMetaRes)
getRideFeedbackMeta a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackMeta a3 a2
