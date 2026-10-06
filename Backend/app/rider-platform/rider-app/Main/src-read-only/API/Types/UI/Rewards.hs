{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Rewards where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardUnlock
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data ClaimCouponResp = ClaimCouponResp
  { couponCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponValidTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    redemptionTargetType :: Domain.Types.RewardCampaign.RedemptionTargetType,
    redemptionTargetUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveRewardCard = LiveRewardCard
  { bgGradientColors :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    header :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    image :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lottie :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subtitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subtitleInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    textColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveRewardCardResp = LiveRewardCardResp {card :: Kernel.Prelude.Maybe LiveRewardCard}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RewardUnlockSummary = RewardUnlockSummary
  { campaignName :: Kernel.Prelude.Text,
    claimedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cohortName :: Kernel.Prelude.Text,
    couponCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponValidTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    presentation :: Kernel.Prelude.Maybe Data.Aeson.Value,
    redemptionTargetType :: Domain.Types.RewardCampaign.RedemptionTargetType,
    redemptionTargetUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rewardImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rewardTitle :: Kernel.Prelude.Text,
    sponsorLogoUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsorName :: Kernel.Prelude.Text,
    status :: Domain.Types.RewardUnlock.UnlockStatus,
    unlockId :: Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock,
    unlockedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
