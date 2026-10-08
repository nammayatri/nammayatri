{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.RideFeedback
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.RideFeedback
import qualified Domain.Action.Dashboard.RideFeedback
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

type API = ("rideFeedback" :> PostRideFeedbackBookingResponseRetryActions)

type PostRideFeedbackBookingResponseRetryActions =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS"
      :> API.Types.RiderPlatform.Management.RideFeedback.PostRideFeedbackBookingResponseRetryActions
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRideFeedbackBookingResponseRetryActions merchantId city

postRideFeedbackBookingResponseRetryActions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideFeedbackBookingResponseRetryActions a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_MANAGEMENT/RIDE_FEEDBACK/POST_RIDE_FEEDBACK_BOOKING_RESPONSE_RETRY_ACTIONS" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.RideFeedback.postRideFeedbackBookingResponseRetryActions a5 a4 a2 a1
    )
