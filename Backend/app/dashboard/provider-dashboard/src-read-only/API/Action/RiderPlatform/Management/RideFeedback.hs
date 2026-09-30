{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.RideFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.RideFeedback
import qualified Dashboard.Common
import qualified Data.Text
import qualified Domain.Action.RiderPlatform.Management.RideFeedback
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.RideFeedbackConfig
import qualified Domain.Types.RideFeedbackResponse
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()

type API = ("rideFeedback" :> (GetRideFeedbackConfigList :<|> GetRideFeedbackConfig :<|> PostRideFeedbackConfigCreate :<|> PostRideFeedbackConfigUpdate :<|> PostRideFeedbackConfigToggle :<|> PostRideFeedbackConfigClone :<|> PostRideFeedbackConfigValidate :<|> GetRideFeedbackRidePreview :<|> GetRideFeedbackRideResponses :<|> PostRideFeedbackResponseRetryActions :<|> GetRideFeedbackMeta))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideFeedbackConfigList merchantId city :<|> getRideFeedbackConfig merchantId city :<|> postRideFeedbackConfigCreate merchantId city :<|> postRideFeedbackConfigUpdate merchantId city :<|> postRideFeedbackConfigToggle merchantId city :<|> postRideFeedbackConfigClone merchantId city :<|> postRideFeedbackConfigValidate merchantId city :<|> getRideFeedbackRidePreview merchantId city :<|> getRideFeedbackRideResponses merchantId city :<|> postRideFeedbackResponseRetryActions merchantId city :<|> getRideFeedbackMeta merchantId city

type GetRideFeedbackConfigList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.GET_RIDE_FEEDBACK_CONFIG_LIST))
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackConfigList
  )

type GetRideFeedbackConfig =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.GET_RIDE_FEEDBACK_CONFIG))
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackConfig
  )

type PostRideFeedbackConfigCreate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.POST_RIDE_FEEDBACK_CONFIG_CREATE))
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigCreate
  )

type PostRideFeedbackConfigUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.POST_RIDE_FEEDBACK_CONFIG_UPDATE))
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigUpdate
  )

type PostRideFeedbackConfigToggle =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.POST_RIDE_FEEDBACK_CONFIG_TOGGLE))
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigToggle
  )

type PostRideFeedbackConfigClone =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.POST_RIDE_FEEDBACK_CONFIG_CLONE))
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigClone
  )

type PostRideFeedbackConfigValidate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.POST_RIDE_FEEDBACK_CONFIG_VALIDATE))
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackConfigValidate
  )

type GetRideFeedbackRidePreview =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.GET_RIDE_FEEDBACK_RIDE_PREVIEW))
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackRidePreview
  )

type GetRideFeedbackRideResponses =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.GET_RIDE_FEEDBACK_RIDE_RESPONSES))
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackRideResponses
  )

type PostRideFeedbackResponseRetryActions =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.POST_RIDE_FEEDBACK_RESPONSE_RETRY_ACTIONS))
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackResponseRetryActions
  )

type GetRideFeedbackMeta =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.RIDE_FEEDBACK) / ('API.Types.RiderPlatform.Management.RideFeedback.GET_RIDE_FEEDBACK_META))
      :> API.Types.RiderPlatform.Management.RideFeedback.GetRideFeedbackMeta
  )

getRideFeedbackConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigListRes)
getRideFeedbackConfigList merchantShortId opCity apiTokenInfo questionKey enabled limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.getRideFeedbackConfigList merchantShortId opCity apiTokenInfo questionKey enabled limit offset

getRideFeedbackConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigItem)
getRideFeedbackConfig merchantShortId opCity apiTokenInfo configId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.getRideFeedbackConfig merchantShortId opCity apiTokenInfo configId

postRideFeedbackConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.postRideFeedbackConfigCreate merchantShortId opCity apiTokenInfo req

postRideFeedbackConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.UpdateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigUpdate merchantShortId opCity apiTokenInfo configId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.postRideFeedbackConfigUpdate merchantShortId opCity apiTokenInfo configId req

postRideFeedbackConfigToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.ToggleRideFeedbackConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackConfigToggle merchantShortId opCity apiTokenInfo configId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.postRideFeedbackConfigToggle merchantShortId opCity apiTokenInfo configId req

postRideFeedbackConfigClone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigRes)
postRideFeedbackConfigClone merchantShortId opCity apiTokenInfo configId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.postRideFeedbackConfigClone merchantShortId opCity apiTokenInfo configId req

postRideFeedbackConfigValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.ValidateRideFeedbackConfigRes)
postRideFeedbackConfigValidate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.postRideFeedbackConfigValidate merchantShortId opCity apiTokenInfo req

getRideFeedbackRidePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackPreviewRes)
getRideFeedbackRidePreview merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.getRideFeedbackRidePreview merchantShortId opCity apiTokenInfo rideId

getRideFeedbackRideResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler [API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackResponseItem])
getRideFeedbackRideResponses merchantShortId opCity apiTokenInfo rideId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.getRideFeedbackRideResponses merchantShortId opCity apiTokenInfo rideId

postRideFeedbackResponseRetryActions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackResponseRetryActions merchantShortId opCity apiTokenInfo responseId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.postRideFeedbackResponseRetryActions merchantShortId opCity apiTokenInfo responseId

getRideFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackMetaRes)
getRideFeedbackMeta merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.RideFeedback.getRideFeedbackMeta merchantShortId opCity apiTokenInfo
