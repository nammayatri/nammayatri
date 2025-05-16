{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.StationFare where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data StationFareT f = StationFareT
  { currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    fareAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    fromStationId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    toStationId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table StationFareT where
  data PrimaryKey StationFareT f = StationFareId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StationFareId . id

type StationFare = StationFareT Identity

$(enableKVPG (''StationFareT) [('id)] [])

$(mkTableInstances (''StationFareT) "station_fares")
