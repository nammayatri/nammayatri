{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.WalkLegMultimodal where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.WalkLegMultimodal
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data WalkLegMultimodalT f = WalkLegMultimodalT
  { distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fromLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    agency :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    convenienceCost :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    isDeleted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    journeyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    journeyLegOrder :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    onSearchFailed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    pricingId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    skipBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    riderId :: B.C f Kernel.Prelude.Text,
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.WalkLegMultimodal.WalkLegStatus,
    toLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table WalkLegMultimodalT where
  data PrimaryKey WalkLegMultimodalT f = WalkLegMultimodalId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = WalkLegMultimodalId . id

type WalkLegMultimodal = WalkLegMultimodalT Identity

$(enableKVPG ''WalkLegMultimodalT ['id] [])

$(mkTableInstances ''WalkLegMultimodalT "walk_leg_multimodal")
