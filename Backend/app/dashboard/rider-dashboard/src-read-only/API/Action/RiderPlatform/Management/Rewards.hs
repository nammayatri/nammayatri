{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management.Rewards
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Rewards
import qualified Dashboard.Common
import qualified Dashboard.RiderPlatform.Management.Rewards
import qualified Domain.Action.RiderPlatform.Management.Rewards
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("rewards" :> (PostRewardsCampaign :<|> PutRewardsCampaign :<|> PostRewardsCampaignCohort :<|> PostRewardsCampaignCohortCodes :<|> PostRewardsCampaignStatus :<|> GetRewardsCampaign :<|> GetRewardsCampaigns :<|> GetRewardsCampaignStats :<|> PostRewardsTriggerEval))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRewardsCampaign merchantId city :<|> putRewardsCampaign merchantId city :<|> postRewardsCampaignCohort merchantId city :<|> postRewardsCampaignCohortCodes merchantId city :<|> postRewardsCampaignStatus merchantId city :<|> getRewardsCampaign merchantId city :<|> getRewardsCampaigns merchantId city :<|> getRewardsCampaignStats merchantId city :<|> postRewardsTriggerEval merchantId city

type PostRewardsCampaign =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.POST_REWARDS_CAMPAIGN))
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaign
  )

type PutRewardsCampaign =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.PUT_REWARDS_CAMPAIGN))
      :> API.Types.RiderPlatform.Management.Rewards.PutRewardsCampaign
  )

type PostRewardsCampaignCohort =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.POST_REWARDS_CAMPAIGN_COHORT))
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaignCohort
  )

type PostRewardsCampaignCohortCodes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.POST_REWARDS_CAMPAIGN_COHORT_CODES))
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaignCohortCodes
  )

type PostRewardsCampaignStatus =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.POST_REWARDS_CAMPAIGN_STATUS))
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaignStatus
  )

type GetRewardsCampaign =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.GET_REWARDS_CAMPAIGN))
      :> API.Types.RiderPlatform.Management.Rewards.GetRewardsCampaign
  )

type GetRewardsCampaigns =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.GET_REWARDS_CAMPAIGNS))
      :> API.Types.RiderPlatform.Management.Rewards.GetRewardsCampaigns
  )

type GetRewardsCampaignStats =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.GET_REWARDS_CAMPAIGN_STATS))
      :> API.Types.RiderPlatform.Management.Rewards.GetRewardsCampaignStats
  )

type PostRewardsTriggerEval =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_MANAGEMENT) / ('API.Types.RiderPlatform.Management.REWARDS) / ('API.Types.RiderPlatform.Management.Rewards.POST_REWARDS_TRIGGER_EVAL))
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsTriggerEval
  )

postRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.Rewards.CreateCampaignReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CreateCampaignResp)
postRewardsCampaign merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.postRewardsCampaign merchantShortId opCity apiTokenInfo req

putRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.EditCampaignReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putRewardsCampaign merchantShortId opCity apiTokenInfo campaignId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.putRewardsCampaign merchantShortId opCity apiTokenInfo campaignId req

postRewardsCampaignCohort :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.CreateCohortReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CreateCohortResp)
postRewardsCampaignCohort merchantShortId opCity apiTokenInfo campaignId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.postRewardsCampaignCohort merchantShortId opCity apiTokenInfo campaignId req

postRewardsCampaignCohortCodes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort -> Dashboard.RiderPlatform.Management.Rewards.UploadCodesReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.UploadCodesResp)
postRewardsCampaignCohortCodes merchantShortId opCity apiTokenInfo campaignId cohortId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.postRewardsCampaignCohortCodes merchantShortId opCity apiTokenInfo campaignId cohortId req

postRewardsCampaignStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.SetStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRewardsCampaignStatus merchantShortId opCity apiTokenInfo campaignId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.postRewardsCampaignStatus merchantShortId opCity apiTokenInfo campaignId req

getRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CampaignDetails)
getRewardsCampaign merchantShortId opCity apiTokenInfo campaignId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.getRewardsCampaign merchantShortId opCity apiTokenInfo campaignId

getRewardsCampaigns :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Rewards.CampaignDetails])
getRewardsCampaigns merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.getRewardsCampaigns merchantShortId opCity apiTokenInfo

getRewardsCampaignStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CampaignStats)
getRewardsCampaignStats merchantShortId opCity apiTokenInfo campaignId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.getRewardsCampaignStats merchantShortId opCity apiTokenInfo campaignId

postRewardsTriggerEval :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRewardsTriggerEval merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.Management.Rewards.postRewardsTriggerEval merchantShortId opCity apiTokenInfo personId
