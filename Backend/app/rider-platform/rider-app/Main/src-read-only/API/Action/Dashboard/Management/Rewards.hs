{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Rewards
  ( API.Types.RiderPlatform.Management.Rewards.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Rewards.API)
handler merchantId city = postRewardsCampaign merchantId city :<|> putRewardsCampaign merchantId city :<|> postRewardsCampaignCohort merchantId city :<|> postRewardsCampaignCohortCodes merchantId city :<|> postRewardsCampaignStatus merchantId city :<|> getRewardsCampaign merchantId city :<|> getRewardsCampaigns merchantId city :<|> getRewardsCampaignStats merchantId city :<|> postRewardsTriggerEval merchantId city

postRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Rewards.CreateCampaignReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CreateCampaignResp)
postRewardsCampaign a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaign a3 a2 a1

putRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.EditCampaignReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putRewardsCampaign a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.putRewardsCampaign a4 a3 a2 a1

postRewardsCampaignCohort :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.CreateCohortReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CreateCohortResp)
postRewardsCampaignCohort a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaignCohort a4 a3 a2 a1

postRewardsCampaignCohortCodes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCohort -> Dashboard.RiderPlatform.Management.Rewards.UploadCodesReq -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.UploadCodesResp)
postRewardsCampaignCohortCodes a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaignCohortCodes a5 a4 a3 a2 a1

postRewardsCampaignStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> API.Types.RiderPlatform.Management.Rewards.SetStatusReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRewardsCampaignStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsCampaignStatus a4 a3 a2 a1

getRewardsCampaign :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CampaignDetails)
getRewardsCampaign a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.getRewardsCampaign a3 a2 a1

getRewardsCampaigns :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler [API.Types.RiderPlatform.Management.Rewards.CampaignDetails])
getRewardsCampaigns a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.getRewardsCampaigns a2 a1

getRewardsCampaignStats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.RiderPlatform.Management.Rewards.RewardCampaign -> Environment.FlowHandler API.Types.RiderPlatform.Management.Rewards.CampaignStats)
getRewardsCampaignStats a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.getRewardsCampaignStats a3 a2 a1

postRewardsTriggerEval :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRewardsTriggerEval a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Rewards.postRewardsTriggerEval a3 a2 a1
