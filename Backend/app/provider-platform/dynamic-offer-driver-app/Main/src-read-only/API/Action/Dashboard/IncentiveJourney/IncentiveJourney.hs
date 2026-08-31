{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.IncentiveJourney.IncentiveJourney
  ( API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import qualified Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> getIncentiveJourneyDriverAssignments merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> getIncentiveJourneyCohortList merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> deleteIncentiveJourneyCohortJourney merchantId city :<|> getIncentiveJourneyCohortJourneyList merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city :<|> postIncentiveJourneyAssignBulkFromS3 merchantId city :<|> getIncentiveJourneyAssignBulkFromS3List merchantId city

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyList a7 a6 a5 a4 a3 a2 a1

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCreate a3 a2 a1

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyUpdate a3 a2 a1

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyMilestoneList a5 a4 a3 a2 a1

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyMilestoneCreate a3 a2 a1

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyMilestoneUpdate a3 a2 a1

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyStatsHistory a8 a7 a6 a5 a4 a3 a2 a1

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyStatsWaiveOff a3 a2 a1

getIncentiveJourneyDriverAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyDriverAssignmentListRes)
getIncentiveJourneyDriverAssignments a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyDriverAssignments a3 a2 a1

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortCreate a3 a2 a1

getIncentiveJourneyCohortList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortDetailsListRes)
getIncentiveJourneyCohortList a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortList a6 a5 a4 a3 a2 a1

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate a3 a2 a1

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate a3 a2 a1

deleteIncentiveJourneyCohortJourney :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyCohortJourney a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyCohortJourney a3 a2 a1

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CohortDetails) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortJourneyList a9 a8 a7 a6 a5 a4 a3 a2 a1

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssign a3 a2 a1

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyUnassign a3 a2 a1

postIncentiveJourneyAssignBulkFromS3 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Req -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Res)
postIncentiveJourneyAssignBulkFromS3 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssignBulkFromS3 a3 a2 a1

getIncentiveJourneyAssignBulkFromS3List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkUserCohortMappingRunStatus -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3ListRes)
getIncentiveJourneyAssignBulkFromS3List a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyAssignBulkFromS3List a5 a4 a3 a2 a1
