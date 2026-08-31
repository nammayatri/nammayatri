{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.IncentiveJourney.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.IncentiveJourney
import qualified API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import qualified Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> GetIncentiveJourneyPersonAssignments :<|> PostIncentiveJourneyCohortCreate :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> DeleteIncentiveJourneyCohortJourney :<|> GetIncentiveJourneyCohortJourneyList :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> getIncentiveJourneyPersonAssignments merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> deleteIncentiveJourneyCohortJourney merchantId city :<|> getIncentiveJourneyCohortJourneyList merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city

type GetIncentiveJourneyList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_LIST)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_CREATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PUT_INCENTIVE_JOURNEY_UPDATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_MILESTONE_LIST)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_MILESTONE_CREATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

type GetIncentiveJourneyStatsHistory =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_STATS_HISTORY)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyStatsHistory
  )

type PostIncentiveJourneyStatsWaiveOff =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyStatsWaiveOff
  )

type GetIncentiveJourneyPersonAssignments =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_PERSON_ASSIGNMENTS)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyPersonAssignments
  )

type PostIncentiveJourneyCohortCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_CREATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortCreate
  )

type PostIncentiveJourneyCohortJourneyCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortJourneyCreate
  )

type PutIncentiveJourneyCohortJourneyUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyCohortJourneyUpdate
  )

type DeleteIncentiveJourneyCohortJourney =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyCohortJourney
  )

type GetIncentiveJourneyCohortJourneyList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_COHORT_JOURNEY_LIST)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyCohortJourneyList
  )

type PostIncentiveJourneyAssign =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_ASSIGN)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyAssign
  )

type DeleteIncentiveJourneyUnassign =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.DELETE_INCENTIVE_JOURNEY_UNASSIGN)
      :> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyUnassign
  )

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled journeyId journeyType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled journeyId journeyType

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo journeyId limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo journeyId limit offset

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo personId journeyId limit offset fromDate toDate = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo personId journeyId limit offset fromDate toDate

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req

getIncentiveJourneyPersonAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyPersonAssignmentListRes)
getIncentiveJourneyPersonAssignments merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyPersonAssignments merchantShortId opCity apiTokenInfo personId

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req

deleteIncentiveJourneyCohortJourney :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyCohortJourney merchantShortId opCity apiTokenInfo cohortJourneyMappingId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyCohortJourney merchantShortId opCity apiTokenInfo cohortJourneyMappingId

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList merchantShortId opCity apiTokenInfo limit offset cohortName cohortCategory isActive journeyType = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortJourneyList merchantShortId opCity apiTokenInfo limit offset cohortName cohortCategory isActive journeyType

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req
