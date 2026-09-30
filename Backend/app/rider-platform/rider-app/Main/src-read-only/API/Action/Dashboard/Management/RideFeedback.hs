{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.RideFeedback
  ( API.Types.RiderPlatform.Management.RideFeedback.API,
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
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.RideFeedback.API)
handler merchantId city = getRideFeedbackConfigList merchantId city :<|> getRideFeedbackConfig merchantId city :<|> postRideFeedbackConfigCreate merchantId city :<|> postRideFeedbackConfigUpdate merchantId city :<|> postRideFeedbackConfigToggle merchantId city :<|> postRideFeedbackConfigClone merchantId city :<|> postRideFeedbackConfigValidate merchantId city :<|> getRideFeedbackRidePreview merchantId city :<|> getRideFeedbackRideResponses merchantId city :<|> postRideFeedbackResponseRetryActions merchantId city :<|> getRideFeedbackMeta merchantId city

getRideFeedbackConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigListRes)
getRideFeedbackConfigList a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackConfigList a6 a5 a4 a3 a2 a1

getRideFeedbackConfig :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigItem)
getRideFeedbackConfig a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackConfig a3 a2 a1

postRideFeedbackConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigCreate a3 a2 a1

postRideFeedbackConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.UpdateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackConfigUpsertRes)
postRideFeedbackConfigUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigUpdate a4 a3 a2 a1

postRideFeedbackConfigToggle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.ToggleRideFeedbackConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackConfigToggle a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigToggle a4 a3 a2 a1

postRideFeedbackConfigClone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RideFeedbackConfig.RideFeedbackConfig -> API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.CloneRideFeedbackConfigRes)
postRideFeedbackConfigClone a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigClone a4 a3 a2 a1

postRideFeedbackConfigValidate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.RideFeedback.CreateRideFeedbackConfigReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.ValidateRideFeedbackConfigRes)
postRideFeedbackConfigValidate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackConfigValidate a3 a2 a1

getRideFeedbackRidePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackPreviewRes)
getRideFeedbackRidePreview a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackRidePreview a3 a2 a1

getRideFeedbackRideResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler [API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackResponseItem])
getRideFeedbackRideResponses a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackRideResponses a3 a2 a1

postRideFeedbackResponseRetryActions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RideFeedbackResponse.RideFeedbackResponse -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackResponseRetryActions a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackResponseRetryActions a3 a2 a1

getRideFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.RiderPlatform.Management.RideFeedback.RideFeedbackMetaRes)
getRideFeedbackMeta a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideFeedback.getRideFeedbackMeta a2 a1
