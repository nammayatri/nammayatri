{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RewardUnlock where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardUnlock
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RewardUnlockT f = RewardUnlockT
  { campaignId :: (B.C f Kernel.Prelude.Text),
    claimedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    cohortId :: (B.C f Kernel.Prelude.Text),
    couponCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    couponSource :: (B.C f Domain.Types.RewardCampaign.CouponSourceType),
    couponValidTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    reclaimedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    redeemedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Domain.Types.RewardUnlock.UnlockStatus),
    unlockSeq :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    unlockedAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    viewedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table RewardUnlockT where
  data PrimaryKey RewardUnlockT f = RewardUnlockId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RewardUnlockId . id

type RewardUnlock = RewardUnlockT Identity

$(enableKVPG (''RewardUnlockT) [('id)] [[('campaignId)], [('personId)]])

$(mkTableInstances (''RewardUnlockT) "reward_unlock")
