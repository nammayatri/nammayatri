{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Rewards
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Rewards
import qualified Dashboard.Common
import qualified Dashboard.RiderPlatform.Management.Rewards
import qualified Domain.Action.Dashboard.Rewards
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("rewards" :> (PostRewardsCampaign :<|> PutRewardsCampaign :<|> PostRewardsCampaignCohort :<|> PutRewardsCampaignCohort :<|> PostRewardsCampaignCohortCodes :<|> PostRewardsCampaignStatus :<|> GetRewardsCampaign :<|> GetRewardsCampaigns :<|> GetRewardsCampaignStats :<|> PostRewardsTriggerEval :<|> PostRewardsCohortValidateEligibility))

type PostRewardsCampaign = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN" :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaign)

type PutRewardsCampaign = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/REWARDS/PUT_REWARDS_CAMPAIGN" :> API.Types.RiderPlatform.Management.Rewards.PutRewardsCampaign)

type PostRewardsCampaignCohort =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_COHORT"
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaignCohort
  )

type PutRewardsCampaignCohort =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/REWARDS/PUT_REWARDS_CAMPAIGN_COHORT"
      :> API.Types.RiderPlatform.Management.Rewards.PutRewardsCampaignCohort
  )

type PostRewardsCampaignCohortCodes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_COHORT_CODES"
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaignCohortCodes
  )

type PostRewardsCampaignStatus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_STATUS"
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCampaignStatus
  )

type GetRewardsCampaign = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGN" :> API.Types.RiderPlatform.Management.Rewards.GetRewardsCampaign)

type GetRewardsCampaigns = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGNS" :> API.Types.RiderPlatform.Management.Rewards.GetRewardsCampaigns)

type GetRewardsCampaignStats = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGN_STATS" :> API.Types.RiderPlatform.Management.Rewards.GetRewardsCampaignStats)

type PostRewardsTriggerEval = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/REWARDS/POST_REWARDS_TRIGGER_EVAL" :> API.Types.RiderPlatform.Management.Rewards.PostRewardsTriggerEval)

type PostRewardsCohortValidateEligibility =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/REWARDS/POST_REWARDS_COHORT_VALIDATE_ELIGIBILITY"
      :> API.Types.RiderPlatform.Management.Rewards.PostRewardsCohortValidateEligibility
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRewardsCampaign merchantId city :<|> putRewardsCampaign merchantId city :<|> postRewardsCampaignCohort merchantId city :<|> putRewardsCampaignCohort merchantId city :<|> postRewardsCampaignCohortCodes merchantId city :<|> postRewardsCampaignStatus merchantId city :<|> getRewardsCampaign merchantId city :<|> getRewardsCampaigns merchantId city :<|> getRewardsCampaignStats merchantId city :<|> postRewardsTriggerEval merchantId city :<|> postRewardsCohortValidateEligibility merchantId city

postRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Rewards.CreateCampaignReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CreateCampaignResp)
postRewardsCampaign a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaign a4 a3 a1

putRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.EditCampaignReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putRewardsCampaign a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.putRewardsCampaign a5 a4 a2 a1

postRewardsCampaignCohort :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.CreateCohortReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CreateCohortResp)
postRewardsCampaignCohort a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaignCohort a5 a4 a2 a1

putRewardsCampaignCohort :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort -> API.Types.RiderPlatform.Management.Rewards.EditCohortReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putRewardsCampaignCohort a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.putRewardsCampaignCohort a6 a5 a3 a2 a1

postRewardsCampaignCohortCodes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort -> Dashboard.RiderPlatform.Management.Rewards.UploadCodesReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.UploadCodesResp)
postRewardsCampaignCohortCodes a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaignCohortCodes a6 a5 a3 a2 a1

postRewardsCampaignStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.SetStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRewardsCampaignStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaignStatus a5 a4 a2 a1

getRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CampaignDetails)
getRewardsCampaign a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.getRewardsCampaign a4 a3 a1

getRewardsCampaigns :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Rewards.CampaignDetails])
getRewardsCampaigns a3 a2 _a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.getRewardsCampaigns a3 a2

getRewardsCampaignStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CampaignStats)
getRewardsCampaignStats a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.getRewardsCampaignStats a4 a3 a1

postRewardsTriggerEval :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRewardsTriggerEval a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsTriggerEval a4 a3 a1

postRewardsCohortValidateEligibility :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Rewards.ValidateCohortEligibilityReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.ValidateCohortEligibilityResp)
postRewardsCohortValidateEligibility a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCohortValidateEligibility a4 a3 a1
