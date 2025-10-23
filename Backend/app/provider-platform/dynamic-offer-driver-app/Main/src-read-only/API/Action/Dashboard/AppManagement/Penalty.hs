{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Penalty
  ( API.Types.Dashboard.AppManagement.Penalty.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Penalty.API)
handler merchantId city = postPenaltyTriggerJobCancellationPenaltyServiceName merchantId city

postPenaltyTriggerJobCancellationPenaltyServiceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPenaltyTriggerJobCancellationPenaltyServiceName a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Penalty.postPenaltyTriggerJobCancellationPenaltyServiceName a3 a2 a1
