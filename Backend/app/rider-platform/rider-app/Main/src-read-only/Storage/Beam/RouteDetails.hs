{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.JourneyModule.State.Types
import Tools.Beam.UtilsTH

data RouteDetailsT f = RouteDetailsT
  { agencyGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    agencyName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    alternateRouteIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    alternateShortNames :: B.C f [Kernel.Prelude.Text],
    endLocationLat :: B.C f Kernel.Prelude.Double,
    endLocationLon :: B.C f Kernel.Prelude.Double,
    frequency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fromArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    fromStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromStopPlatformCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    journeyLegId :: B.C f Kernel.Prelude.Text,
    legEndTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    legStartTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    routeCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeColorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeColorName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeLongName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routeShortName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    startLocationLat :: B.C f Kernel.Prelude.Double,
    startLocationLon :: B.C f Kernel.Prelude.Double,
    subLegOrder :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    toArrivalTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toDepartureTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    toStopCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopGtfsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toStopPlatformCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    trackingStatus :: B.C f (Kernel.Prelude.Maybe Lib.JourneyModule.State.Types.TrackingStatus),
    trackingStatusLastUpdatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    userBookedRouteShortName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteDetailsT where
  data PrimaryKey RouteDetailsT f = RouteDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteDetailsId . id

type RouteDetails = RouteDetailsT Identity

$(enableKVPG ''RouteDetailsT ['id] [['journeyLegId]])

$(mkTableInstances ''RouteDetailsT "route_details")
