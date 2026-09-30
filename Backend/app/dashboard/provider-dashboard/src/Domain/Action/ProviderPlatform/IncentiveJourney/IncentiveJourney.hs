module Domain.Action.ProviderPlatform.IncentiveJourney.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    getIncentiveJourneyStatsHistory,
    getIncentiveJourneyDriverAssignments,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    getIncentiveJourneyCohortList,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    getIncentiveJourneyCohortJourneyList,
    postIncentiveJourneyAssign,
    postIncentiveJourneyUnassign,
    postIncentiveJourneyAssignBulkFromS3,
    getIncentiveJourneyAssignBulkFromS3List,
    postIncentiveJourneyAutoApplyCohortCreate,
    putIncentiveJourneyAutoApplyCohortUpdate,
    getIncentiveJourneyAutoApplyCohortList,
  )
where

import qualified API.Client.ProviderPlatform.IncentiveJourney
import qualified API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified Domain.Types.VehicleCategory
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset journeyId journeyType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyList) limit offset journeyId journeyType

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCreate) req)

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo journeyId limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyMilestoneList) journeyId limit offset

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyMilestoneCreate) req)

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo driverId journeyId limit offset fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyStatsHistory) driverId journeyId limit offset fromDate toDate

getIncentiveJourneyDriverAssignments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyDriverAssignmentListRes)
getIncentiveJourneyDriverAssignments merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyDriverAssignments) driverId

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyStatsWaiveOff) req)

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsReq -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCohortCreate) req)

getIncentiveJourneyCohortList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortDetailsListRes)
getIncentiveJourneyCohortList merchantShortId opCity apiTokenInfo limit offset cohortName cohortCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyCohortList) limit offset cohortName cohortCategory

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCohortJourneyCreate) req)

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyCohortJourneyUpdate) req)

getIncentiveJourneyCohortJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.CohortDetails) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.IncentiveJourneyType -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CohortJourneyMappingListRes)
getIncentiveJourneyCohortJourneyList merchantShortId opCity apiTokenInfo limit offset cohortName cohortId cohortCategory isActive journeyType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyCohortJourneyList) limit offset cohortName cohortId cohortCategory isActive journeyType

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyAssign) req)

postIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyUnassign) req)

postIncentiveJourneyAssignBulkFromS3 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Req -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3Res)
postIncentiveJourneyAssignBulkFromS3 merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyAssignBulkFromS3) req)

getIncentiveJourneyAssignBulkFromS3List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkUserCohortMappingRunStatus -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.BulkAssignUserCohortFromS3ListRes)
getIncentiveJourneyAssignBulkFromS3List merchantShortId opCity apiTokenInfo limit offset status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyAssignBulkFromS3List) limit offset status

postIncentiveJourneyAutoApplyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateAutoApplyCohortMappingReq -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.CreateAutoApplyCohortMappingRes)
postIncentiveJourneyAutoApplyCohortCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyAutoApplyCohortCreate) req)

putIncentiveJourneyAutoApplyCohortUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.UpdateAutoApplyCohortMappingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyAutoApplyCohortUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyAutoApplyCohortUpdate) req)

getIncentiveJourneyAutoApplyCohortList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.ProviderPlatform.IncentiveJourney.IncentiveJourney.AutoApplyCohortMappingListRes)
getIncentiveJourneyAutoApplyCohortList merchantShortId opCity apiTokenInfo limit offset vehicleCategory enabled = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.IncentiveJourney.callIncentiveJourneyAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyAutoApplyCohortList) limit offset vehicleCategory enabled
