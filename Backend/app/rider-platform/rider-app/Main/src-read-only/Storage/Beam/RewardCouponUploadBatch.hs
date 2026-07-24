{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RewardCouponUploadBatch where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RewardCouponUploadBatchT f = RewardCouponUploadBatchT
  { campaignId :: (B.C f Kernel.Prelude.Text),
    cohortId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    s3Key :: (B.C f Kernel.Prelude.Text),
    totalCodesUploaded :: (B.C f Kernel.Prelude.Int),
    uploadBatchId :: (B.C f Kernel.Prelude.Text),
    uploadedAt :: (B.C f Kernel.Prelude.UTCTime),
    uploadedBy :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RewardCouponUploadBatchT where
  data PrimaryKey RewardCouponUploadBatchT f = RewardCouponUploadBatchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RewardCouponUploadBatchId . id

type RewardCouponUploadBatch = RewardCouponUploadBatchT Identity

$(enableKVPG (''RewardCouponUploadBatchT) [('id)] [[('campaignId)], [('cohortId)]])

$(mkTableInstances (''RewardCouponUploadBatchT) "reward_coupon_upload_batch")
