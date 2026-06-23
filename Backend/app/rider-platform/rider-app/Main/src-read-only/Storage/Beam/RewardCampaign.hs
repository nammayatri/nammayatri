{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RewardCampaign where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RewardCampaign
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RewardCampaignT f = RewardCampaignT
  { claimMode :: (B.C f Domain.Types.RewardCampaign.ClaimMode),
    couponSourceType :: (B.C f Domain.Types.RewardCampaign.CouponSourceType),
    couponTemplate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    createdBy :: (B.C f Kernel.Prelude.Text),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    displayOrder :: (B.C f Kernel.Prelude.Int),
    endsAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    reclaimPolicy :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    redemptionTargetType :: (B.C f Domain.Types.RewardCampaign.RedemptionTargetType),
    redemptionTargetUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sponsorLogoUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sponsorName :: (B.C f Kernel.Prelude.Text),
    sponsorType :: (B.C f Domain.Types.RewardCampaign.SponsorType),
    startsAt :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Domain.Types.RewardCampaign.CampaignStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RewardCampaignT where
  data PrimaryKey RewardCampaignT f = RewardCampaignId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RewardCampaignId . id

type RewardCampaign = RewardCampaignT Identity

$(enableKVPG (''RewardCampaignT) [('id)] [])

$(mkTableInstances (''RewardCampaignT) "reward_campaign")
