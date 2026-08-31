{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.IncentiveJourney.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney
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

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> GetIncentiveJourneyPersonAssignments :<|> PostIncentiveJourneyCohortCreate :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> DeleteIncentiveJourneyCohortJourney :<|> GetIncentiveJourneyCohortJourneyList :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign))

type GetIncentiveJourneyList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_LIST"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_MILESTONE_LIST"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

type GetIncentiveJourneyStatsHistory =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_STATS_HISTORY"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyStatsHistory
  )

type PostIncentiveJourneyStatsWaiveOff =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyStatsWaiveOff
  )

type GetIncentiveJourneyPersonAssignments =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_PERSON_ASSIGNMENTS"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyPersonAssignments
  )

type PostIncentiveJourneyCohortCreate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_CREATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortCreate
  )

type PostIncentiveJourneyCohortJourneyCreate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortJourneyCreate
  )

type PutIncentiveJourneyCohortJourneyUpdate =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyCohortJourneyUpdate
  )

type DeleteIncentiveJourneyCohortJourney =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyCohortJourney
  )

type GetIncentiveJourneyCohortJourneyList =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/GET_INCENTIVE_JOURNEY_COHORT_JOURNEY_LIST"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyCohortJourneyList
  )

type PostIncentiveJourneyAssign =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyAssign
  )

type DeleteIncentiveJourneyUnassign =
  ( DashboardUserAuth
      'APP_BACKEND_MANAGEMENT
      "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_UNASSIGN"
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyUnassign
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> getIncentiveJourneyPersonAssignments merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> deleteIncentiveJourneyCohortJourney merchantId city :<|> getIncentiveJourneyCohortJourneyList merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyList a8 a7 a5 a4 a3 a2 a1

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCreate a4 a3 a1
    )

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyUpdate a4 a3 a1
    )

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyMilestoneList a6 a5 a3 a2 a1

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_MILESTONE_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyMilestoneCreate a4 a3 a1
    )

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyMilestoneUpdate a4 a3 a1
    )

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyStatsHistory a9 a8 a6 a5 a4 a3 a2 a1

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyStatsWaiveOff a4 a3 a1
    )

getIncentiveJourneyPersonAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyPersonAssignmentListRes)
getIncentiveJourneyPersonAssignments a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyPersonAssignments a4 a3 a1

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortCreate a4 a3 a1
    )

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate a4 a3 a1
    )

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate a4 a3 a1
    )

deleteIncentiveJourneyCohortJourney :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyCohortJourney a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyCohortJourney a4 a3 a1
    )

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortJourneyList a9 a8 a6 a5 a4 a3 a2 a1

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/POST_INCENTIVE_JOURNEY_ASSIGN" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssign a4 a3 a1
    )

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_INCENTIVE_JOURNEY/INCENTIVE_JOURNEY/DELETE_INCENTIVE_JOURNEY_UNASSIGN" a2 (Kernel.Prelude.Just a1)
        Domain.Action.Dashboard.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyUnassign a4 a3 a1
    )
