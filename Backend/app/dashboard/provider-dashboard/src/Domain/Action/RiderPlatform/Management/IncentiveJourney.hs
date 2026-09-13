{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.IncentiveJourney
  ( getIncentiveJourneyList,
    postIncentiveJourneyCreate,
    putIncentiveJourneyUpdate,
    getIncentiveJourneyMilestoneList,
    postIncentiveJourneyMilestoneCreate,
    putIncentiveJourneyMilestoneUpdate,
    getIncentiveJourneyStatsHistory,
    postIncentiveJourneyStatsWaiveOff,
    postIncentiveJourneyCohortCreate,
    postIncentiveJourneyCohortJourneyCreate,
    putIncentiveJourneyCohortJourneyUpdate,
    postIncentiveJourneyAssign,
    deleteIncentiveJourneyUnassign,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.IncentiveJourney
import qualified Dashboard.Common
import qualified Data.Time
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyList) limit offset enabled

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCreate) req)

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyUpdate) req)

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyMilestoneList) limit offset journeyId

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyMilestoneCreate) req)

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyMilestoneUpdate) req)

getIncentiveJourneyStatsHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Kernel.Types.Id.Id Dashboard.Common.Person -> Data.Time.Day -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.IncentiveJourneyStatsHistoryRes)
getIncentiveJourneyStatsHistory merchantShortId opCity apiTokenInfo journeyId limit offset fromDate personId toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.getIncentiveJourneyStatsHistory) journeyId limit offset fromDate personId toDate

postIncentiveJourneyStatsWaiveOff :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.WaiveIncentiveJourneyMilestoneReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyStatsWaiveOff merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyStatsWaiveOff) req)

postIncentiveJourneyCohortCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortDetailsReq -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortDetailsRes)
postIncentiveJourneyCohortCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCohortCreate) req)

postIncentiveJourneyCohortJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingReq -> Environment.Flow API.Types.RiderPlatform.Management.IncentiveJourney.CreateCohortJourneyMappingRes)
postIncentiveJourneyCohortJourneyCreate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyCohortJourneyCreate) req)

putIncentiveJourneyCohortJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UpdateCohortJourneyMappingReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyCohortJourneyUpdate merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.putIncentiveJourneyCohortJourneyUpdate) req)

postIncentiveJourneyAssign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.AssignUserToIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postIncentiveJourneyAssign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.postIncentiveJourneyAssign) req)

deleteIncentiveJourneyUnassign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.IncentiveJourney.UnassignUserFromIncentiveJourneyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteIncentiveJourneyUnassign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.incentiveJourneyDSL.deleteIncentiveJourneyUnassign) req)
