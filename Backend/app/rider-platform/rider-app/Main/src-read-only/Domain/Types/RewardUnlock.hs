{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RewardUnlock where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RewardCampaign
import qualified Domain.Types.RewardCohort
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data RewardUnlock = RewardUnlock
  { campaignId :: Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign,
    claimedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cohortId :: Kernel.Types.Id.Id Domain.Types.RewardCohort.RewardCohort,
    couponCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    couponSource :: Domain.Types.RewardCampaign.CouponSourceType,
    couponValidTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    reclaimedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    redeemedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Domain.Types.RewardUnlock.UnlockStatus,
    unlockedAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    viewedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, (Show), (Eq))

data UnlockStatus = Active | Redeemed | ExpiredUnredeemed | Reclaimed deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''UnlockStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''UnlockStatus))
