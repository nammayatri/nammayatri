{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RewardOffer where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RewardOffer = RewardOffer
  { active :: Kernel.Prelude.Bool,
    description :: Kernel.Prelude.Maybe Data.Text.Text,
    displayOrder :: Kernel.Prelude.Int,
    entityType :: Domain.Types.RewardOffer.RewardEntityType,
    id :: Kernel.Types.Id.Id Domain.Types.RewardOffer.RewardOffer,
    imageUrl :: Kernel.Prelude.Maybe Data.Text.Text,
    logicDomain :: Data.Text.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    milestoneTarget :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    requiredTags :: [Data.Text.Text],
    title :: Data.Text.Text,
    triggerEvent :: Domain.Types.RewardOffer.RewardTriggerEvent,
    validFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data RewardEntityType = DRIVER | RIDER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data RewardTriggerEvent
  = EndRide
  | Rating
  | Cancellation
  | DriverToCustomerReferral
  | CustomerToDriverReferral
  | LeaderBoard
  | Training
  | BulkUploadEvent
  | LMS
  | LMSBonus
  | BookingComplete
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RewardEntityType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RewardTriggerEvent))
