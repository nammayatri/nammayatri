{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RewardOffer where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RewardOffer
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RewardOfferT f = RewardOfferT
  { active :: B.C f Kernel.Prelude.Bool,
    description :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    displayOrder :: B.C f Kernel.Prelude.Int,
    entityType :: B.C f Domain.Types.RewardOffer.RewardEntityType,
    id :: B.C f Data.Text.Text,
    imageUrl :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    logicDomain :: B.C f Data.Text.Text,
    merchantId :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    milestoneTarget :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    requiredTags :: B.C f [Data.Text.Text],
    title :: B.C f Data.Text.Text,
    triggerEvent :: B.C f Domain.Types.RewardOffer.RewardTriggerEvent,
    validFrom :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    validTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RewardOfferT where
  data PrimaryKey RewardOfferT f = RewardOfferId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = RewardOfferId . id

type RewardOffer = RewardOfferT Identity

$(enableKVPG ''RewardOfferT ['id] [])

$(mkTableInstances ''RewardOfferT "reward_offer")
