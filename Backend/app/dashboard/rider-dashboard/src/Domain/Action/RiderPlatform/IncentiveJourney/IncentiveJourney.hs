{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.IncentiveJourney.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    putIncentiveJourneyUpdate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    putIncentiveJourneyMilestoneUpdate,
    getIncentiveJourneyStatsHistory,
    getIncentiveJourneyPersonAssignments,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    deleteIncentiveJourneyCohortJourney,
    getIncentiveJourneyCohortJourneyList,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
  )
where

import qualified API.Client.RiderPlatform.IncentiveJourney
import qualified API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled journeyId journeyType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyList) limit offset enabled journeyId journeyType

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCreate) req)

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyUpdate) req)

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo journeyId limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyMilestoneList) journeyId limit offset

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyMilestoneCreate) req)

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyMilestoneUpdate) req)

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Person -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo personId journeyId limit offset fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyStatsHistory) personId journeyId limit offset fromDate toDate

getIncentiveJourneyPersonAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyPersonAssignmentListRes)
getIncentiveJourneyPersonAssignments merchantShortId opCity apiTokenInfo personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyPersonAssignments) personId

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyStatsWaiveOff) req)

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCohortCreate) req)

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCohortJourneyCreate) req)

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyCohortJourneyUpdate) req)

deleteIncentiveJourneyCohortJourney :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.CohortJourneyMapping -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyCohortJourney merchantShortId opCity apiTokenInfo cohortJourneyMappingId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.deleteIncentiveJourneyCohortJourney) cohortJourneyMappingId)

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.Flow API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList merchantShortId opCity apiTokenInfo limit offset cohortName cohortCategory isActive journeyType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyCohortJourneyList) limit offset cohortName cohortCategory isActive journeyType

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyAssign) req)

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.RiderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.deleteIncentiveJourneyUnassign) req)
