{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.JourneyRouteDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Time
import Tools.Beam.UtilsTH

data JourneyRouteDetailsT f = JourneyRouteDetailsT
  { frequency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Seconds),
    fromStationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    lineColor :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    lineColorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    platformNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeLongName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    searchId :: B.C f Kernel.Prelude.Text,
    subLegOrder :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    toStationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyRouteDetailsT where
  data PrimaryKey JourneyRouteDetailsT f = JourneyRouteDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyRouteDetailsId . id

type JourneyRouteDetails = JourneyRouteDetailsT Identity

$(enableKVPG ''JourneyRouteDetailsT ['id] [['searchId]])

$(mkTableInstances ''JourneyRouteDetailsT "journey_route_details")
