{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.Rewards where

import qualified Dashboard.Common
import qualified Dashboard.Common.Rewards.CampaignStatus
import qualified Dashboard.Common.Rewards.ClaimMode
import qualified Dashboard.Common.Rewards.CouponSourceType
import qualified Dashboard.Common.Rewards.RedemptionTargetType
import qualified Dashboard.Common.Rewards.SponsorType
import qualified Dashboard.RiderPlatform.Management.Rewards
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CampaignDetails = CampaignDetails {campaign :: RewardCampaign, cohorts :: [RewardCohort]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CampaignStats = CampaignStats
  { totalUnlocks :: Kernel.Prelude.Int,
    unlocksByCohort :: [CohortCount],
    poolRemainingByCohort :: [CohortPoolRemaining],
    claimRate :: Kernel.Prelude.Double,
    expiredUnredeemedCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CohortCount = CohortCount {cohortId :: Kernel.Types.Id.Id RewardCohort, count :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CohortPoolRemaining = CohortPoolRemaining {cohortId :: Kernel.Types.Id.Id RewardCohort, remaining :: Kernel.Prelude.Integer}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateCampaignReq = CreateCampaignReq
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsorType :: Dashboard.Common.Rewards.SponsorType.SponsorType,
    sponsorName :: Kernel.Prelude.Text,
    sponsorLogoUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponSourceType :: Dashboard.Common.Rewards.CouponSourceType.CouponSourceType,
    couponTemplate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    redemptionTargetType :: Dashboard.Common.Rewards.RedemptionTargetType.RedemptionTargetType,
    redemptionTargetUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    claimMode :: Dashboard.Common.Rewards.ClaimMode.ClaimMode,
    startsAt :: Kernel.Prelude.UTCTime,
    endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    displayOrder :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCampaignReq where
  hideSecrets = Kernel.Prelude.identity

data CreateCampaignResp = CreateCampaignResp {campaignId :: Kernel.Types.Id.Id RewardCampaign}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateCohortReq = CreateCohortReq
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    displayOrder :: Kernel.Prelude.Int,
    eligibilityJsonLogic :: Data.Aeson.Value,
    rewardTitle :: Kernel.Prelude.Text,
    rewardImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponValidityDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    presentation :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCohortReq where
  hideSecrets = Kernel.Prelude.identity

data CreateCohortResp = CreateCohortResp {cohortId :: Kernel.Types.Id.Id RewardCohort}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EditCampaignReq = EditCampaignReq {description :: Kernel.Prelude.Maybe Kernel.Prelude.Text, displayOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int, endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets EditCampaignReq where
  hideSecrets = Kernel.Prelude.identity

data EditCohortReq = EditCohortReq
  { name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    displayOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eligibilityJsonLogic :: Kernel.Prelude.Maybe Data.Aeson.Value,
    rewardTitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rewardImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponValidityDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    presentation :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets EditCohortReq where
  hideSecrets = Kernel.Prelude.identity

data RewardCampaign = RewardCampaign
  { id :: Kernel.Types.Id.Id RewardCampaign,
    merchantId :: Kernel.Types.Id.Id Dashboard.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsorType :: Dashboard.Common.Rewards.SponsorType.SponsorType,
    sponsorName :: Kernel.Prelude.Text,
    sponsorLogoUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponSourceType :: Dashboard.Common.Rewards.CouponSourceType.CouponSourceType,
    couponTemplate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    redemptionTargetType :: Dashboard.Common.Rewards.RedemptionTargetType.RedemptionTargetType,
    redemptionTargetUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    claimMode :: Dashboard.Common.Rewards.ClaimMode.ClaimMode,
    reclaimPolicy :: Kernel.Prelude.Maybe Data.Aeson.Value,
    startsAt :: Kernel.Prelude.UTCTime,
    endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Dashboard.Common.Rewards.CampaignStatus.CampaignStatus,
    displayOrder :: Kernel.Prelude.Int,
    createdBy :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RewardCohort = RewardCohort
  { id :: Kernel.Types.Id.Id RewardCohort,
    campaignId :: Kernel.Types.Id.Id RewardCampaign,
    name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    displayOrder :: Kernel.Prelude.Int,
    eligibilityJsonLogic :: Data.Aeson.Value,
    rewardTitle :: Kernel.Prelude.Text,
    rewardImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponValidityDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    presentation :: Kernel.Prelude.Maybe Data.Aeson.Value,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SetStatusReq = SetStatusReq {newStatus :: Dashboard.Common.Rewards.CampaignStatus.CampaignStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SetStatusReq where
  hideSecrets = Kernel.Prelude.identity

data UploadCodesResp = UploadCodesResp {uploadBatchId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("rewards" :> (PostRewardsCampaign :<|> PutRewardsCampaign :<|> PostRewardsCampaignCohort :<|> PutRewardsCampaignCohort :<|> PostRewardsCampaignCohortCodes :<|> PostRewardsCampaignStatus :<|> GetRewardsCampaign :<|> GetRewardsCampaigns :<|> GetRewardsCampaignStats :<|> PostRewardsTriggerEval))

type PostRewardsCampaign = ("campaign" :> ReqBody ('[JSON]) CreateCampaignReq :> Post ('[JSON]) CreateCampaignResp)

type PutRewardsCampaign = ("campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> ReqBody ('[JSON]) EditCampaignReq :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostRewardsCampaignCohort = ("campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> "cohort" :> ReqBody ('[JSON]) CreateCohortReq :> Post ('[JSON]) CreateCohortResp)

type PutRewardsCampaignCohort =
  ( "campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> "cohort" :> Capture "cohortId" (Kernel.Types.Id.Id RewardCohort)
      :> ReqBody
           ('[JSON])
           EditCohortReq
      :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type PostRewardsCampaignCohortCodes =
  ( "campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> "cohort"
      :> Capture
           "cohortId"
           (Kernel.Types.Id.Id RewardCohort)
      :> "codes"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.RiderPlatform.Management.Rewards.UploadCodesReq
      :> Post
           ('[JSON])
           UploadCodesResp
  )

type PostRewardsCampaignStatus =
  ( "campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> "status" :> ReqBody ('[JSON]) SetStatusReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetRewardsCampaign = ("campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> Get ('[JSON]) CampaignDetails)

type GetRewardsCampaigns = ("campaigns" :> Get ('[JSON]) [CampaignDetails])

type GetRewardsCampaignStats = ("campaign" :> Capture "campaignId" (Kernel.Types.Id.Id RewardCampaign) :> "stats" :> Get ('[JSON]) CampaignStats)

type PostRewardsTriggerEval = ("triggerEval" :> Capture "personId" (Kernel.Types.Id.Id Dashboard.Common.Person) :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data RewardsAPIs = RewardsAPIs
  { postRewardsCampaign :: (CreateCampaignReq -> EulerHS.Types.EulerClient CreateCampaignResp),
    putRewardsCampaign :: (Kernel.Types.Id.Id RewardCampaign -> EditCampaignReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRewardsCampaignCohort :: (Kernel.Types.Id.Id RewardCampaign -> CreateCohortReq -> EulerHS.Types.EulerClient CreateCohortResp),
    putRewardsCampaignCohort :: (Kernel.Types.Id.Id RewardCampaign -> Kernel.Types.Id.Id RewardCohort -> EditCohortReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postRewardsCampaignCohortCodes ::
      ( Kernel.Types.Id.Id RewardCampaign ->
        Kernel.Types.Id.Id RewardCohort ->
        ( Data.ByteString.Lazy.ByteString,
          Dashboard.RiderPlatform.Management.Rewards.UploadCodesReq
        ) ->
        EulerHS.Types.EulerClient UploadCodesResp
      ),
    postRewardsCampaignStatus :: (Kernel.Types.Id.Id RewardCampaign -> SetStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getRewardsCampaign :: (Kernel.Types.Id.Id RewardCampaign -> EulerHS.Types.EulerClient CampaignDetails),
    getRewardsCampaigns :: (EulerHS.Types.EulerClient [CampaignDetails]),
    getRewardsCampaignStats :: (Kernel.Types.Id.Id RewardCampaign -> EulerHS.Types.EulerClient CampaignStats),
    postRewardsTriggerEval :: (Kernel.Types.Id.Id Dashboard.Common.Person -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkRewardsAPIs :: (Client EulerHS.Types.EulerClient API -> RewardsAPIs)
mkRewardsAPIs rewardsClient = (RewardsAPIs {..})
  where
    postRewardsCampaign :<|> putRewardsCampaign :<|> postRewardsCampaignCohort :<|> putRewardsCampaignCohort :<|> postRewardsCampaignCohortCodes :<|> postRewardsCampaignStatus :<|> getRewardsCampaign :<|> getRewardsCampaigns :<|> getRewardsCampaignStats :<|> postRewardsTriggerEval = rewardsClient

data RewardsUserActionType
  = POST_REWARDS_CAMPAIGN
  | PUT_REWARDS_CAMPAIGN
  | POST_REWARDS_CAMPAIGN_COHORT
  | PUT_REWARDS_CAMPAIGN_COHORT
  | POST_REWARDS_CAMPAIGN_COHORT_CODES
  | POST_REWARDS_CAMPAIGN_STATUS
  | GET_REWARDS_CAMPAIGN
  | GET_REWARDS_CAMPAIGNS
  | GET_REWARDS_CAMPAIGN_STATS
  | POST_REWARDS_TRIGGER_EVAL
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''RewardsUserActionType)])
