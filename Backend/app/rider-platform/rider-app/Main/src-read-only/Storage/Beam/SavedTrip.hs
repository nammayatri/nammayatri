{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SavedTrip where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SavedTrip
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SavedTripT f = SavedTripT
  { id :: B.C f Kernel.Prelude.Text,
    riderId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    originLat :: B.C f Kernel.Prelude.Double,
    originLon :: B.C f Kernel.Prelude.Double,
    originAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    destinationLat :: B.C f Kernel.Prelude.Double,
    destinationLon :: B.C f Kernel.Prelude.Double,
    destinationAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    timeMode :: B.C f Domain.Types.SavedTrip.TimeMode,
    targetTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    targetTimeOfDaySeconds :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bufferMinutes :: B.C f Kernel.Prelude.Int,
    recurrence :: B.C f Domain.Types.SavedTrip.TripRecurrence,
    customDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    notifyBeforeMinutes :: B.C f Kernel.Prelude.Int,
    isActive :: B.C f Kernel.Prelude.Bool,
    lastComputedDeparture :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    lastNotifiedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SavedTripT where
  data PrimaryKey SavedTripT f = SavedTripId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SavedTripId . id

type SavedTrip = SavedTripT Identity

$(enableKVPG ''SavedTripT ['id] [['riderId]])

$(mkTableInstances ''SavedTripT "saved_trip")
