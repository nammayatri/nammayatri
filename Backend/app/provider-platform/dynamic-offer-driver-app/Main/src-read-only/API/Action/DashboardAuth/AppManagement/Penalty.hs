{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.Penalty
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Penalty
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.AppManagement.Penalty
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("penalty" :> PostPenaltyTriggerJobCancellationPenaltyServiceName)

type PostPenaltyTriggerJobCancellationPenaltyServiceName =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/PENALTY/POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME"
      :> API.Types.Dashboard.AppManagement.Penalty.PostPenaltyTriggerJobCancellationPenaltyServiceName
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPenaltyTriggerJobCancellationPenaltyServiceName merchantId city

postPenaltyTriggerJobCancellationPenaltyServiceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPenaltyTriggerJobCancellationPenaltyServiceName a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Penalty.postPenaltyTriggerJobCancellationPenaltyServiceName a4 a3 a1
