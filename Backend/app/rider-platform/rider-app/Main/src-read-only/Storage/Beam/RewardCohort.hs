{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RewardCohort where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RewardCohortT f = RewardCohortT
  { campaignId :: (B.C f Kernel.Prelude.Text),
    couponValidityDays :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    displayOrder :: (B.C f Kernel.Prelude.Int),
    eligibilityJsonLogic :: (B.C f Data.Aeson.Value),
    id :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    presentation :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    rewardImageUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rewardTitle :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table RewardCohortT where
  data PrimaryKey RewardCohortT f = RewardCohortId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RewardCohortId . id

type RewardCohort = RewardCohortT Identity

$(enableKVPG (''RewardCohortT) [('id)] [[('campaignId)]])

$(mkTableInstances (''RewardCohortT) "reward_cohort")
