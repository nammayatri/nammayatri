{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MgCampaignWindow where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MgCampaignWindowT f = MgCampaignWindowT
  { campaignId :: B.C f Kernel.Prelude.Text,
    endTime :: B.C f Data.Time.TimeOfDay,
    id :: B.C f Kernel.Prelude.Text,
    startTime :: B.C f Data.Time.TimeOfDay,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MgCampaignWindowT where
  data PrimaryKey MgCampaignWindowT f = MgCampaignWindowId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MgCampaignWindowId . id

type MgCampaignWindow = MgCampaignWindowT Identity

$(enableKVPG ''MgCampaignWindowT ['id] [['campaignId]])

$(mkTableInstances ''MgCampaignWindowT "mg_campaign_window")
