{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.Management.Rewards
  ( postRewardsCampaign,
    putRewardsCampaign,
    postRewardsCampaignCohort,
    putRewardsCampaignCohort,
    postRewardsCampaignCohortCodes,
    postRewardsCampaignStatus,
    getRewardsCampaign,
    getRewardsCampaigns,
    getRewardsCampaignStats,
    postRewardsTriggerEval,
  )
where

import qualified API.Client.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Rewards
import qualified Dashboard.Common
import qualified Dashboard.RiderPlatform.Management.Rewards
import qualified Data.ByteString.Lazy as LBS
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

postRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Rewards.CreateCampaignReq -> Environment.Flow API.Types.RiderPlatform.Management.Rewards.CreateCampaignResp)
postRewardsCampaign merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.postRewardsCampaign) req

putRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.EditCampaignReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putRewardsCampaign merchantShortId opCity apiTokenInfo campaignId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.putRewardsCampaign) campaignId req

postRewardsCampaignCohort :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.CreateCohortReq -> Environment.Flow API.Types.RiderPlatform.Management.Rewards.CreateCohortResp)
postRewardsCampaignCohort merchantShortId opCity apiTokenInfo campaignId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.postRewardsCampaignCohort) campaignId req

putRewardsCampaignCohort :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort -> API.Types.RiderPlatform.Management.Rewards.EditCohortReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putRewardsCampaignCohort merchantShortId opCity apiTokenInfo campaignId cohortId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.putRewardsCampaignCohort) campaignId cohortId req

postRewardsCampaignCohortCodes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort -> Dashboard.RiderPlatform.Management.Rewards.UploadCodesReq -> Environment.Flow API.Types.RiderPlatform.Management.Rewards.UploadCodesResp)
postRewardsCampaignCohortCodes merchantShortId opCity apiTokenInfo campaignId cohortId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (addMultipartBoundary "XXX00XXX" . (.rewardsDSL.postRewardsCampaignCohortCodes)) campaignId cohortId req
  where
    addMultipartBoundary ::
      LBS.ByteString ->
      ( Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign ->
        Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort ->
        (LBS.ByteString, reqBody) ->
        res
      ) ->
      Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign ->
      Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort ->
      reqBody ->
      res
    addMultipartBoundary boundary clientFn campaignId_ cohortId_ reqBody =
      clientFn campaignId_ cohortId_ (boundary, reqBody)

postRewardsCampaignStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.SetStatusReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRewardsCampaignStatus merchantShortId opCity apiTokenInfo campaignId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.postRewardsCampaignStatus) campaignId req

getRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.Flow API.Types.RiderPlatform.Management.Rewards.CampaignDetails)
getRewardsCampaign merchantShortId opCity apiTokenInfo campaignId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.getRewardsCampaign) campaignId

getRewardsCampaigns :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.Flow [API.Types.RiderPlatform.Management.Rewards.CampaignDetails])
getRewardsCampaigns merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.getRewardsCampaigns)

getRewardsCampaignStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.Flow API.Types.RiderPlatform.Management.Rewards.CampaignStats)
getRewardsCampaignStats merchantShortId opCity apiTokenInfo campaignId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.getRewardsCampaignStats) campaignId

postRewardsTriggerEval :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRewardsTriggerEval merchantShortId opCity apiTokenInfo personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    API.Client.RiderPlatform.Management.callManagementAPI checkedMerchantId opCity (.rewardsDSL.postRewardsTriggerEval) personId
