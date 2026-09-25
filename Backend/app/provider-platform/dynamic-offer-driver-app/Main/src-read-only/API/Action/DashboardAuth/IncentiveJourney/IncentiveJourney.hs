{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IncentiveJourney.IncentiveJourney
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> GetIncentiveJourneyDriverAssignments :<|> PostIncentiveJourneyCohortCreate :<|> GetIncentiveJourneyCohortList :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> DeleteIncentiveJourneyCohortJourney :<|> GetIncentiveJourneyCohortJourneyList :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign :<|> PostIncentiveJourneyAssignBulkFromS3 :<|> GetIncentiveJourneyAssignBulkFromS3List))

type GetIncentiveJourneyList =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_LIST"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_MILESTONE_LIST"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

type GetIncentiveJourneyStatsHistory =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_STATS_HISTORY"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyStatsHistory
  )

type PostIncentiveJourneyStatsWaiveOff =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyStatsWaiveOff
  )

type GetIncentiveJourneyDriverAssignments =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_DRIVER_ASSIGNMENTS"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyDriverAssignments
  )

type PostIncentiveJourneyCohortCreate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_CREATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortCreate
  )

type GetIncentiveJourneyCohortList =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_COHORT_LIST"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyCohortList
  )

type PostIncentiveJourneyCohortJourneyCreate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortJourneyCreate
  )

type PutIncentiveJourneyCohortJourneyUpdate =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyCohortJourneyUpdate
  )

type DeleteIncentiveJourneyCohortJourney =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyCohortJourney
  )

type GetIncentiveJourneyCohortJourneyList =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_COHORT_JOURNEY_LIST"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyCohortJourneyList
  )

type PostIncentiveJourneyAssign =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyAssign
  )

type DeleteIncentiveJourneyUnassign =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_UNASSIGN"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyUnassign
  )

type PostIncentiveJourneyAssignBulkFromS3 =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN_BULK_FROM_S3"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyAssignBulkFromS3
  )

type GetIncentiveJourneyAssignBulkFromS3List =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_ASSIGN_BULK_FROM_S3_LIST"
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyAssignBulkFromS3List
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> getIncentiveJourneyDriverAssignments merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> getIncentiveJourneyCohortList merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> deleteIncentiveJourneyCohortJourney merchantId city :<|> getIncentiveJourneyCohortJourneyList merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city :<|> postIncentiveJourneyAssignBulkFromS3 merchantId city :<|> getIncentiveJourneyAssignBulkFromS3List merchantId city

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyList a8 a7 a5 a4 a3 a2 a1

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCreate a4 a3 a1
    )

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyUpdate a4 a3 a1
    )

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyMilestoneList a6 a5 a3 a2 a1

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyMilestoneCreate a4 a3 a1
    )

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyMilestoneUpdate a4 a3 a1
    )

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyStatsHistory a9 a8 a6 a5 a4 a3 a2 a1

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyStatsWaiveOff a4 a3 a1
    )

getIncentiveJourneyDriverAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyDriverAssignmentListRes)
getIncentiveJourneyDriverAssignments a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyDriverAssignments a4 a3 a1

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortCreate a4 a3 a1
    )

getIncentiveJourneyCohortList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortDetailsListRes)
getIncentiveJourneyCohortList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortList a7 a6 a4 a3 a2 a1

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate a4 a3 a1
    )

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate a4 a3 a1
    )

deleteIncentiveJourneyCohortJourney :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyCohortJourney a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyCohortJourney a4 a3 a1
    )

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CohortDetails) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortJourneyList a10 a9 a7 a6 a5 a4 a3 a2 a1

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssign a4 a3 a1
    )

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_UNASSIGN" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyUnassign a4 a3 a1
    )

postIncentiveJourneyAssignBulkFromS3 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Req -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Res)
postIncentiveJourneyAssignBulkFromS3 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN_BULK_FROM_S3" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssignBulkFromS3 a4 a3 a1
    )

getIncentiveJourneyAssignBulkFromS3List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkUserCohortMappingRunStatus -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3ListRes)
getIncentiveJourneyAssignBulkFromS3List a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyAssignBulkFromS3List a6 a5 a3 a2 a1
