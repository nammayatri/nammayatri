{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.Penalty
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Penalty
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.AppManagement.Penalty
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("penalty" :> PostPenaltyTriggerJobCancellationPenaltyServiceName)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postPenaltyTriggerJobCancellationPenaltyServiceName merchantId city

type PostPenaltyTriggerJobCancellationPenaltyServiceName =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PENALTY) / ('API.Types.Dashboard.AppManagement.Penalty.POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME))
      :> API.Types.Dashboard.AppManagement.Penalty.PostPenaltyTriggerJobCancellationPenaltyServiceName
  )

postPenaltyTriggerJobCancellationPenaltyServiceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Dashboard.Common.ServiceNames -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPenaltyTriggerJobCancellationPenaltyServiceName merchantShortId opCity apiTokenInfo serviceName = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.Penalty.postPenaltyTriggerJobCancellationPenaltyServiceName merchantShortId opCity apiTokenInfo serviceName
