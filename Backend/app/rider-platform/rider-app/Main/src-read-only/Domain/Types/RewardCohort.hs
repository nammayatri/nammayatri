{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RewardCohort where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RewardCampaign
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RewardCohort = RewardCohort
  { campaignId :: Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign,
    couponValidityDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    displayOrder :: Kernel.Prelude.Int,
    eligibilityJsonLogic :: Data.Aeson.Value,
    id :: Kernel.Types.Id.Id Domain.Types.RewardCohort.RewardCohort,
    name :: Kernel.Prelude.Text,
    presentation :: Kernel.Prelude.Maybe Data.Aeson.Value,
    rewardImageUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rewardTitle :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, (Show), (Eq))
