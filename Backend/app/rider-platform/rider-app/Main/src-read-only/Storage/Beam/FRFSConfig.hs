{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Time
import Tools.Beam.UtilsTH

data FRFSConfigT f = FRFSConfigT
  { bookingEndTime :: B.C f Kernel.Prelude.UTCTime,
    bookingStartTime :: B.C f Kernel.Prelude.UTCTime,
    busStationTtl :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Seconds),
    cancellationReasonId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    customDates :: B.C f [Kernel.Prelude.Text],
    customEndTime :: B.C f Kernel.Prelude.Text,
    discount :: B.C f Kernel.Prelude.Int,
    freeTicketInterval :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    id :: B.C f Kernel.Prelude.Text,
    isCancellationAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isEventOngoing :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxFreeTicketCashback :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    metroStationTtl :: B.C f Kernel.Prelude.Int,
    oneWayTicketLimit :: B.C f Kernel.Prelude.Int,
    providerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    providerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    radius :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    roundTripTicketLimit :: B.C f Kernel.Prelude.Int,
    routeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    straightLineDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    validTillSeconds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Seconds),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSConfigT where
  data PrimaryKey FRFSConfigT f = FRFSConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSConfigId . id

type FRFSConfig = FRFSConfigT Identity

$(enableKVPG ''FRFSConfigT ['id] [])

$(mkTableInstances ''FRFSConfigT "frfs_config")
