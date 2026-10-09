{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.RideFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.RideFeedback
import qualified Dashboard.Common
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

type API = ("rideFeedback" :> (GetRideFeedbackRidePreview :<|> GetRideFeedbackRideResponses :<|> GetRideFeedbackMeta))

type GetRideFeedbackRidePreview =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_PREVIEW"
      :> API.Types.ProviderPlatform.Management.RideFeedback.GetRideFeedbackRidePreview
  )

type GetRideFeedbackRideResponses =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_RIDE_RESPONSES"
      :> API.Types.ProviderPlatform.Management.RideFeedback.GetRideFeedbackRideResponses
  )

type GetRideFeedbackMeta =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/RIDE_FEEDBACK/GET_RIDE_FEEDBACK_META"
      :> API.Types.ProviderPlatform.Management.RideFeedback.GetRideFeedbackMeta
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRideFeedbackRidePreview merchantId city :<|> getRideFeedbackRideResponses merchantId city :<|> getRideFeedbackMeta merchantId city

getRideFeedbackRidePreview :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RideFeedback.RideFeedbackPreviewRes)
getRideFeedbackRidePreview a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RideFeedback.getRideFeedbackRidePreview a4 a3 a1

getRideFeedbackRideResponses :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.RideFeedback.RideFeedbackResponseItem])
getRideFeedbackRideResponses a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.Management.RideFeedback.getRideFeedbackRideResponses a4 a3 a1

getRideFeedbackMeta :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RideFeedback.RideFeedbackMetaRes)
getRideFeedbackMeta a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.Management.RideFeedback.getRideFeedbackMeta a3 a2
