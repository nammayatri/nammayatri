{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MgCampaign where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MgCampaign
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data MgCampaignT f = MgCampaignT
  { createdBy :: B.C f Kernel.Prelude.Text,
    currency :: B.C f Kernel.Types.Common.Currency,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    endDate :: B.C f Data.Time.Calendar.Day,
    forceAssign :: B.C f Kernel.Prelude.Bool,
    frequency :: B.C f Domain.Types.MgCampaign.MgFrequency,
    guaranteeAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    id :: B.C f Kernel.Prelude.Text,
    maxBudget :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    onFleetChange :: B.C f Domain.Types.MgCampaign.MgFleetChangePolicy,
    optInCutoffMinutes :: B.C f Kernel.Prelude.Int,
    settlementGraceMinutes :: B.C f Kernel.Prelude.Int,
    startDate :: B.C f Data.Time.Calendar.Day,
    status :: B.C f Domain.Types.MgCampaign.MgCampaignStatus,
    vehicleCategories :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MgCampaignT where
  data PrimaryKey MgCampaignT f = MgCampaignId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MgCampaignId . id

type MgCampaign = MgCampaignT Identity

$(enableKVPG ''MgCampaignT ['id] [])

$(mkTableInstances ''MgCampaignT "mg_campaign")
