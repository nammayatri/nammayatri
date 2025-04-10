{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteStopCalender where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RouteStopCalenderT f = RouteStopCalenderT
  { integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    serviceability :: B.C f [Kernel.Prelude.Int],
    tripId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteStopCalenderT where
  data PrimaryKey RouteStopCalenderT f = RouteStopCalenderId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteStopCalenderId <$> integratedBppConfigId <*> tripId

type RouteStopCalender = RouteStopCalenderT Identity

$(enableKVPG ''RouteStopCalenderT ['integratedBppConfigId, 'tripId] [])

$(mkTableInstances ''RouteStopCalenderT "route_stop_calender")
