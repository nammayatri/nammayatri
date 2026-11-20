{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ShuttleInterest where

import qualified Data.Time
import qualified Data.Time.LocalTime
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ShuttleInterest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ShuttleInterestT f = ShuttleInterestT
  { acPreference :: (B.C f (Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleAcPreference)),
    companyDomain :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Data.Time.UTCTime),
    daysOfWeek :: (B.C f [Kernel.Prelude.Text]),
    dropAddress :: (B.C f Kernel.Prelude.Text),
    dropLat :: (B.C f Kernel.Prelude.Double),
    dropLon :: (B.C f Kernel.Prelude.Double),
    dropTime :: (B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay)),
    maxWalkingDistanceMeters :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    pickupAddress :: (B.C f Kernel.Prelude.Text),
    pickupLat :: (B.C f Kernel.Prelude.Double),
    pickupLon :: (B.C f Kernel.Prelude.Double),
    pickupTime :: (B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay)),
    shiftEndTime :: (B.C f Data.Time.LocalTime.TimeOfDay),
    shiftStartTime :: (B.C f Data.Time.LocalTime.TimeOfDay),
    timeFlexibilityMinutes :: (B.C f Kernel.Prelude.Int),
    tripType :: (B.C f Domain.Types.ShuttleInterest.ShuttleTripType),
    updatedAt :: (B.C f Data.Time.UTCTime),
    vehicleSizePreference :: (B.C f (Kernel.Prelude.Maybe Domain.Types.ShuttleInterest.ShuttleVehicleSizePreference))
  }
  deriving (Generic, B.Beamable)

instance B.Table ShuttleInterestT where
  data PrimaryKey ShuttleInterestT f = ShuttleInterestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ShuttleInterestId . personId

type ShuttleInterest = ShuttleInterestT Identity

$(enableKVPG (''ShuttleInterestT) [('personId)] [])

$(mkTableInstances (''ShuttleInterestT) "shuttle_interest")
