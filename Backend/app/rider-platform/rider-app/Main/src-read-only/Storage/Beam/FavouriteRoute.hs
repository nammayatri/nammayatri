{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FavouriteRoute where

import qualified BecknV2.FRFS.Enums
import qualified Data.Time.LocalTime
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FavouriteRoute
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FavouriteRouteT f = FavouriteRouteT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    favouriteType :: (B.C f Domain.Types.FavouriteRoute.FavouriteRouteType),
    fromStopCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    integratedBppConfigId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    notifyOn :: (B.C f [Domain.Types.FavouriteRoute.FavouriteNotifyType]),
    notifyingStop :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    recurrenceRuleDays :: (B.C f [Kernel.Prelude.Text]),
    riderId :: (B.C f Kernel.Prelude.Text),
    routeCode :: (B.C f Kernel.Prelude.Text),
    routeShortName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    serviceTierType :: (B.C f (Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType)),
    shouldNotify :: (B.C f Kernel.Prelude.Bool),
    status :: (B.C f Domain.Types.FavouriteRoute.FavouriteRouteStatus),
    toStopCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    tripId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleType :: (B.C f BecknV2.FRFS.Enums.VehicleCategory),
    windowEndTime :: (B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay)),
    windowStartTime :: (B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay))
  }
  deriving (Generic, B.Beamable)

instance B.Table FavouriteRouteT where
  data PrimaryKey FavouriteRouteT f = FavouriteRouteId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FavouriteRouteId . id

type FavouriteRoute = FavouriteRouteT Identity

$(enableKVPG (''FavouriteRouteT) [('id)] [[('notifyingStop)], [('riderId)], [('routeCode)], [('tripId)]])

$(mkTableInstances (''FavouriteRouteT) "favourite_route")
