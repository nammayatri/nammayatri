{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedBooking where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedBooking
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SharedBookingT f = SharedBookingT
  { bookingIds :: (B.C f [Kernel.Prelude.Text]),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    distanceUnit :: (B.C f Kernel.Types.Common.DistanceUnit),
    driverId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    estimatedDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    estimatedTotalFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    fromLocationIds :: (B.C f [Kernel.Prelude.Text]),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    pairingTime :: (B.C f Kernel.Prelude.UTCTime),
    providerId :: (B.C f Kernel.Prelude.Text),
    providerUrl :: (B.C f Kernel.Prelude.Text),
    sharedEstimateId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SharedBooking.BookingStatus),
    toLocationIds :: (B.C f [Kernel.Prelude.Text]),
    tollNames :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    transactionId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleServiceTier :: (B.C f Domain.Types.ServiceTierType.ServiceTierType)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedBookingT where
  data PrimaryKey SharedBookingT f = SharedBookingId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedBookingId . id

type SharedBooking = SharedBookingT Identity

$(enableKVPG (''SharedBookingT) [('id)] [[('sharedEstimateId)]])

$(mkTableInstances (''SharedBookingT) "shared_booking")
