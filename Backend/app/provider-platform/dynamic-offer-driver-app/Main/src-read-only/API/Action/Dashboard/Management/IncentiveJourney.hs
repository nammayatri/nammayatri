{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.IncentiveJourney
  ( API.Types.ProviderPlatform.Management.IncentiveJourney.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.IncentiveJourney.API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.getIncentiveJourneyList a5 a4 a3 a2 a1

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyCreate a3 a2 a1

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.putIncentiveJourneyUpdate a3 a2 a1

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.getIncentiveJourneyMilestoneList a5 a4 a3 a2 a1

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyMilestoneCreate a3 a2 a1

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.putIncentiveJourneyMilestoneUpdate a3 a2 a1

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.getIncentiveJourneyStatsHistory a8 a7 a6 a5 a4 a3 a2 a1

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyStatsWaiveOff a3 a2 a1

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyCohortCreate a3 a2 a1

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate a3 a2 a1

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate a3 a2 a1

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.postIncentiveJourneyAssign a3 a2 a1

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.IncentiveJourney.deleteIncentiveJourneyUnassign a3 a2 a1
