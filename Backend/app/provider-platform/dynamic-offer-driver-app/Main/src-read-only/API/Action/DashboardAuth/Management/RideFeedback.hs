{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.RideFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.RideFeedback
import qualified Data.Text
import qualified Domain.Action.Dashboard.Management.RideFeedback
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("rideFeedback" :> (GetRideFeedbackInspectPreview :<|> GetRideFeedbackInspectResponses :<|> GetRideFeedbackMeta))

type GetRideFeedbackInspectPreview =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_INSPECT_PREVIEW"
      :> API.Types.ProviderPlatform.Management.RideFeedback.GetRideFeedbackInspectPreview
  )

type GetRideFeedbackInspectResponses =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_INSPECT_RESPONSES"
      :> API.Types.ProviderPlatform.Management.RideFeedback.GetRideFeedbackInspectResponses
  )

type GetRideFeedbackMeta =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_META"
      :> API.Types.ProviderPlatform.Management.RideFeedback.GetRideFeedbackMeta
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideFeedbackInspectPreview merchantId city :<|> getRideFeedbackInspectResponses merchantId city :<|> getRideFeedbackMeta merchantId city

getRideFeedbackInspectPreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RideFeedback.RideFeedbackPreviewRes)
getRideFeedbackInspectPreview a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RideFeedback.getRideFeedbackInspectPreview a4 a3 a1

getRideFeedbackInspectResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Data.Text.Text -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.RideFeedback.RideFeedbackResponseItem])
getRideFeedbackInspectResponses a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RideFeedback.getRideFeedbackInspectResponses a4 a3 a1

getRideFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RideFeedback.RideFeedbackMetaRes)
getRideFeedbackMeta a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.Management.RideFeedback.getRideFeedbackMeta a3 a2
