{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.BookingCancellationReason where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.CancellationReason
import qualified Domain.Types.BookingCancellationReason
import qualified Database.Beam as B



data BookingCancellationReasonT f
    = BookingCancellationReasonT {additionalInfo :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  bookingId :: (B.C f Kernel.Prelude.Text),
                                  createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                  distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                                  driverCancellationLocationLat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                                  driverCancellationLocationLon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                                  driverDistToPickup :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
                                  driverDistToPickupValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
                                  merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                  reasonCode :: (B.C f (Kernel.Prelude.Maybe Domain.Types.CancellationReason.CancellationReasonCode)),
                                  reasonStage :: (B.C f (Kernel.Prelude.Maybe Domain.Types.CancellationReason.CancellationStage)),
                                  rideId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                  riderId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                  source :: (B.C f Domain.Types.BookingCancellationReason.CancellationSource),
                                  updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime))}
    deriving (Generic, B.Beamable)
instance B.Table BookingCancellationReasonT
    where data PrimaryKey BookingCancellationReasonT f = BookingCancellationReasonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = BookingCancellationReasonId . bookingId
type BookingCancellationReason = BookingCancellationReasonT Identity

$(enableKVPG (''BookingCancellationReasonT) [('bookingId)] [])

$(mkTableInstances (''BookingCancellationReasonT) "booking_cancellation_reason")

