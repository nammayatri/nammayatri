{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MgCampaignRule where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MgCampaignRule
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MgCampaignRuleT f = MgCampaignRuleT
  { campaignId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    metric :: B.C f Domain.Types.MgCampaignRule.MgMetric,
    params :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    slabs :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MgCampaignRuleT where
  data PrimaryKey MgCampaignRuleT f = MgCampaignRuleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MgCampaignRuleId . id

type MgCampaignRule = MgCampaignRuleT Identity

$(enableKVPG ''MgCampaignRuleT ['id] [['campaignId]])

$(mkTableInstances ''MgCampaignRuleT "mg_campaign_rule")
