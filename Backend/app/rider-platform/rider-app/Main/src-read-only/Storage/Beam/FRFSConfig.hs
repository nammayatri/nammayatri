{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSConfig where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSConfigT f = FRFSConfigT
  { bookingEndTime :: B.C f Kernel.Prelude.UTCTime,
    bookingStartTime :: B.C f Kernel.Prelude.UTCTime,
    customDates :: B.C f [Kernel.Prelude.Text],
    customEndTime :: B.C f Kernel.Prelude.Text,
    discount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    metroStationTtl :: B.C f Kernel.Prelude.Int,
    oneWayTicketLimit :: B.C f Kernel.Prelude.Int,
    roundTripTicketLimit :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSConfigT where
  data PrimaryKey FRFSConfigT f = FRFSConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSConfigId . merchantOperatingCityId

type FRFSConfig = FRFSConfigT Identity

$(enableKVPG ''FRFSConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''FRFSConfigT "frfs_config")
