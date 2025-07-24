{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyBooking where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data JourneyBookingT f = JourneyBookingT
  { convenienceCost :: B.C f Kernel.Prelude.Int,
    customerCancelled :: B.C f Kernel.Prelude.Bool,
    distanceUnit :: B.C f Kernel.Types.Common.DistanceUnit,
    estimatedDistance :: B.C f Kernel.Types.Common.HighPrecDistance,
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    id :: B.C f Kernel.Prelude.Text,
    isBookingCancellable :: B.C f Kernel.Prelude.Bool,
    journeyId :: B.C f Kernel.Prelude.Text,
    modes :: B.C f [Domain.Types.Common.MultimodalTravelMode],
    numberOfPassengers :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyBookingT where
  data PrimaryKey JourneyBookingT f = JourneyBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyBookingId . id

type JourneyBooking = JourneyBookingT Identity

$(enableKVPG ''JourneyBookingT ['id] [['journeyId]])

$(mkTableInstances ''JourneyBookingT "journey_booking")
