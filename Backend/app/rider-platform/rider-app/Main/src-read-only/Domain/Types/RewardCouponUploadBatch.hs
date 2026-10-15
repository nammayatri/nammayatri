{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RewardCouponUploadBatch where

import Data.Aeson
import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardCohort
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RewardCouponUploadBatch = RewardCouponUploadBatch
  { campaignId :: Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign,
    cohortId :: Kernel.Types.Id.Id Domain.Types.RewardCohort.RewardCohort,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RewardCouponUploadBatch.RewardCouponUploadBatch,
    s3Key :: Kernel.Prelude.Text,
    totalCodesUploaded :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    uploadBatchId :: Kernel.Prelude.Text,
    uploadedAt :: Kernel.Prelude.UTCTime,
    uploadedBy :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, Eq)
