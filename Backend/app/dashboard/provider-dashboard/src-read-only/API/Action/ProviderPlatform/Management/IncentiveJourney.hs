{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import qualified Domain.Action.ProviderPlatform.Management.IncentiveJourney
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> PostIncentiveJourneyCohortCreate :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city

type GetIncentiveJourneyList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_LIST)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_CREATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_UPDATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_MILESTONE_LIST)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_MILESTONE_CREATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

type GetIncentiveJourneyStatsHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_STATS_HISTORY)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyStatsHistory
  )

type PostIncentiveJourneyStatsWaiveOff =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyStatsWaiveOff
  )

type PostIncentiveJourneyCohortCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_CREATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCohortCreate
  )

type PostIncentiveJourneyCohortJourneyCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCohortJourneyCreate
  )

type PutIncentiveJourneyCohortJourneyUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyCohortJourneyUpdate
  )

type PostIncentiveJourneyAssign =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_ASSIGN)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyAssign
  )

type DeleteIncentiveJourneyUnassign =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.Management.IncentiveJourney.DELETE_INCENTIVE_JOURNEY_UNASSIGN)
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.DeleteIncentiveJourneyUnassign
  )

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo journeyId limit offset driverId fromDate toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo journeyId limit offset driverId fromDate toDate

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req
