{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.IncentiveJourney
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.IncentiveJourney
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate))

type GetIncentiveJourneyList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_LIST"
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE"
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE"
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_MILESTONE_LIST"
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE"
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE"
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.getIncentiveJourneyList a7 a6 a4 a3 a2 a1

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyCreate a4 a3 a1

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.putIncentiveJourneyUpdate a4 a3 a1

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.getIncentiveJourneyMilestoneList a6 a5 a3 a2 a1

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyMilestoneCreate a4 a3 a1

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.putIncentiveJourneyMilestoneUpdate a4 a3 a1
