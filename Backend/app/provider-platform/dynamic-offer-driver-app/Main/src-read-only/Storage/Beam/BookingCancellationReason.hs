{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BookingCancellationReason where

import qualified Database.Beam as B
import qualified Domain.Types.BookingCancellationReason
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data BookingCancellationReasonT f = BookingCancellationReasonT
  { additionalInfo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    bookingId :: B.C f Kernel.Prelude.Text,
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverCancellationLocationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driverCancellationLocationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driverDistToPickup :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    driverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    reasonCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rideId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    source :: B.C f Domain.Types.BookingCancellationReason.CancellationSource
  }
  deriving (Generic, B.Beamable)

instance B.Table BookingCancellationReasonT where
  data PrimaryKey BookingCancellationReasonT f = BookingCancellationReasonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BookingCancellationReasonId . bookingId

type BookingCancellationReason = BookingCancellationReasonT Identity

$(enableKVPG ''BookingCancellationReasonT ['bookingId] [['rideId]])

$(mkTableInstances ''BookingCancellationReasonT "booking_cancellation_reason")
