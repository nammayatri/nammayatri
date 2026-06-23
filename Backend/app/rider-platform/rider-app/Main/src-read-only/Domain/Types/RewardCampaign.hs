{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RewardCampaign where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data RewardCampaign = RewardCampaign
  { claimMode :: Domain.Types.RewardCampaign.ClaimMode,
    couponSourceType :: Domain.Types.RewardCampaign.CouponSourceType,
    couponTemplate :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    displayOrder :: Kernel.Prelude.Int,
    endsAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RewardCampaign.RewardCampaign,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Text,
    reclaimPolicy :: Kernel.Prelude.Maybe Data.Aeson.Value,
    redemptionTargetType :: Domain.Types.RewardCampaign.RedemptionTargetType,
    redemptionTargetUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsorLogoUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sponsorName :: Kernel.Prelude.Text,
    sponsorType :: Domain.Types.RewardCampaign.SponsorType,
    startsAt :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.RewardCampaign.CampaignStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, (Show), (Eq))

data CampaignStatus = Draft | Active | Paused | Ended deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data ClaimMode = AutoClaim | ManualClaim deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data CouponSourceType = Pool | Templated deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data RedemptionTargetType = InApp | ExternalUrl | ExternalManual deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

data SponsorType = Internal | External deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SponsorType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''SponsorType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''CouponSourceType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''CouponSourceType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''RedemptionTargetType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''RedemptionTargetType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ClaimMode))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''ClaimMode))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''CampaignStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''CampaignStatus))
