{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import qualified Domain.Action.RiderPlatform.Management.IncentiveJourney
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
import Tools.Auth.Api

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> PostIncentiveJourneyCohortCreate :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city

type GetIncentiveJourneyList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_LIST)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_CREATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_UPDATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_MILESTONE_LIST)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_MILESTONE_CREATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

type GetIncentiveJourneyStatsHistory =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_STATS_HISTORY)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.GetIncentiveJourneyStatsHistory
  )

type PostIncentiveJourneyStatsWaiveOff =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PostIncentiveJourneyStatsWaiveOff
  )

type PostIncentiveJourneyCohortCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_CREATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCohortCreate
  )

type PostIncentiveJourneyCohortJourneyCreate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCohortJourneyCreate
  )

type PutIncentiveJourneyCohortJourneyUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PutIncentiveJourneyCohortJourneyUpdate
  )

type PostIncentiveJourneyAssign =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_ASSIGN)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.PostIncentiveJourneyAssign
  )

type DeleteIncentiveJourneyUnassign =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_MANAGEMENT / 'API.Types.RiderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.RiderPlatform.Management.IncentiveJourney.DELETE_INCENTIVE_JOURNEY_UNASSIGN)
      :> API.Types.RiderPlatform.Management.IncentiveJourney.DeleteIncentiveJourneyUnassign
  )

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Kernel.Types.Id.Id Dashboard.Common.Person -> Data.Time.Day -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo journeyId limit offset fromDate personId toDate = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo journeyId limit offset fromDate personId toDate

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.IncentiveJourney.deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req
