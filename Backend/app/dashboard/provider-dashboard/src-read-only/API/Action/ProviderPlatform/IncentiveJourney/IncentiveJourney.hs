{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.IncentiveJourney
import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import qualified Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
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

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate :<|> GetIncentiveJourneyStatsHistory :<|> PostIncentiveJourneyStatsWaiveOff :<|> GetIncentiveJourneyDriverAssignments :<|> PostIncentiveJourneyCohortCreate :<|> GetIncentiveJourneyCohortList :<|> PostIncentiveJourneyCohortJourneyCreate :<|> PutIncentiveJourneyCohortJourneyUpdate :<|> DeleteIncentiveJourneyCohortJourney :<|> GetIncentiveJourneyCohortJourneyList :<|> PostIncentiveJourneyAssign :<|> DeleteIncentiveJourneyUnassign :<|> PostIncentiveJourneyAssignBulkFromS3 :<|> GetIncentiveJourneyAssignBulkFromS3List))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city :<|> getIncentiveJourneyStatsHistory merchantId city :<|> postIncentiveJourneyStatsWaiveOff merchantId city :<|> getIncentiveJourneyDriverAssignments merchantId city :<|> postIncentiveJourneyCohortCreate merchantId city :<|> getIncentiveJourneyCohortList merchantId city :<|> postIncentiveJourneyCohortJourneyCreate merchantId city :<|> putIncentiveJourneyCohortJourneyUpdate merchantId city :<|> deleteIncentiveJourneyCohortJourney merchantId city :<|> getIncentiveJourneyCohortJourneyList merchantId city :<|> postIncentiveJourneyAssign merchantId city :<|> deleteIncentiveJourneyUnassign merchantId city :<|> postIncentiveJourneyAssignBulkFromS3 merchantId city :<|> getIncentiveJourneyAssignBulkFromS3List merchantId city

type GetIncentiveJourneyList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_LIST)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_CREATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PUT_INCENTIVE_JOURNEY_UPDATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_MILESTONE_LIST)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_MILESTONE_CREATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

type GetIncentiveJourneyStatsHistory =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_STATS_HISTORY)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyStatsHistory
  )

type PostIncentiveJourneyStatsWaiveOff =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_STATS_WAIVE_OFF)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyStatsWaiveOff
  )

type GetIncentiveJourneyDriverAssignments =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_DRIVER_ASSIGNMENTS)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyDriverAssignments
  )

type PostIncentiveJourneyCohortCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_CREATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortCreate
  )

type GetIncentiveJourneyCohortList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_COHORT_LIST)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyCohortList
  )

type PostIncentiveJourneyCohortJourneyCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_COHORT_JOURNEY_CREATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyCohortJourneyCreate
  )

type PutIncentiveJourneyCohortJourneyUpdate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PUT_INCENTIVE_JOURNEY_COHORT_JOURNEY_UPDATE)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PutIncentiveJourneyCohortJourneyUpdate
  )

type DeleteIncentiveJourneyCohortJourney =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.DELETE_INCENTIVE_JOURNEY_COHORT_JOURNEY)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyCohortJourney
  )

type GetIncentiveJourneyCohortJourneyList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_COHORT_JOURNEY_LIST)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyCohortJourneyList
  )

type PostIncentiveJourneyAssign =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_ASSIGN)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyAssign
  )

type DeleteIncentiveJourneyUnassign =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.DELETE_INCENTIVE_JOURNEY_UNASSIGN)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.DeleteIncentiveJourneyUnassign
  )

type PostIncentiveJourneyAssignBulkFromS3 =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.POST_INCENTIVE_JOURNEY_ASSIGN_BULK_FROM_S3)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.PostIncentiveJourneyAssignBulkFromS3
  )

type GetIncentiveJourneyAssignBulkFromS3List =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.INCENTIVE_JOURNEY / 'API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GET_INCENTIVE_JOURNEY_ASSIGN_BULK_FROM_S3_LIST)
      :> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.GetIncentiveJourneyAssignBulkFromS3List
  )

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled journeyId journeyType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled journeyId journeyType

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo journeyId limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo journeyId limit offset

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo driverId journeyId limit offset fromDate toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo driverId journeyId limit offset fromDate toDate

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req

getIncentiveJourneyDriverAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyDriverAssignmentListRes)
getIncentiveJourneyDriverAssignments merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyDriverAssignments merchantShortId opCity apiTokenInfo driverId

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyCohortList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortDetailsListRes)
getIncentiveJourneyCohortList merchantShortId opCity apiTokenInfo limit offset cohortName cohortCategory = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortList merchantShortId opCity apiTokenInfo limit offset cohortName cohortCategory

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req

deleteIncentiveJourneyCohortJourney :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyCohortJourney merchantShortId opCity apiTokenInfo cohortJourneyMappingId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyCohortJourney merchantShortId opCity apiTokenInfo cohortJourneyMappingId

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CohortDetails) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList merchantShortId opCity apiTokenInfo limit offset cohortName cohortId cohortCategory isActive journeyType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyCohortJourneyList merchantShortId opCity apiTokenInfo limit offset cohortName cohortId cohortCategory isActive journeyType

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req

postIncentiveJourneyAssignBulkFromS3 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Req -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Res)
postIncentiveJourneyAssignBulkFromS3 merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.postIncentiveJourneyAssignBulkFromS3 merchantShortId opCity apiTokenInfo req

getIncentiveJourneyAssignBulkFromS3List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkUserCohortMappingRunStatus -> Environment.FlowHandler API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3ListRes)
getIncentiveJourneyAssignBulkFromS3List merchantShortId opCity apiTokenInfo limit offset status = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney.getIncentiveJourneyAssignBulkFromS3List merchantShortId opCity apiTokenInfo limit offset status
