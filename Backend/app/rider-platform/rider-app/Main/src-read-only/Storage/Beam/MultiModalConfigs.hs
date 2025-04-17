{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MultiModalConfigs where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Time
import Tools.Beam.UtilsTH

data MultiModalConfigsT f = MultiModalConfigsT
  { busFilterTimeBufferInSeconds :: B.C f Kernel.Types.Time.Seconds,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    enableBusFiltering :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    id :: B.C f Kernel.Prelude.Text,
    makeMultiModalSearch :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxAllowedPublicTransportLegs :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maximumWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    metroBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    minimumWalkDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    multimodalTesting :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    nearbyDriverSearchRadius :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    permissibleModes :: B.C f (Kernel.Prelude.Maybe [Kernel.External.MultiModal.Interface.Types.GeneralVehicleType]),
    straightLineThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    suburbanBookingAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MultiModalConfigsT where
  data PrimaryKey MultiModalConfigsT f = MultiModalConfigsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MultiModalConfigsId . id

type MultiModalConfigs = MultiModalConfigsT Identity

$(enableKVPG ''MultiModalConfigsT ['id] [])

$(mkTableInstances ''MultiModalConfigsT "multi_modal_configs")
