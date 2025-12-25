{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyLegMapping where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data JourneyLegMappingT f = JourneyLegMappingT
  { id :: B.C f Kernel.Prelude.Text,
    isDeleted :: B.C f Kernel.Prelude.Bool,
    journeyId :: B.C f Kernel.Prelude.Text,
    journeyLegId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    sequenceNumber :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyLegMappingT where
  data PrimaryKey JourneyLegMappingT f = JourneyLegMappingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyLegMappingId . id

type JourneyLegMapping = JourneyLegMappingT Identity

$(enableKVPG ''JourneyLegMappingT ['id] [['journeyId], ['journeyLegId]])

$(mkTableInstances ''JourneyLegMappingT "journey_leg_mapping")
