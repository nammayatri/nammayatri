{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.OccupancyAlert where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.OccupancyAlert
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data OccupancyAlertT f = OccupancyAlertT
  { acknowledgedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    category :: (B.C f Domain.Types.OccupancyAlert.AlertCategory),
    detectedAt :: (B.C f Kernel.Prelude.UTCTime),
    detectionSourceId :: (B.C f Kernel.Prelude.Text),
    externalAlertId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    graceUntil :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    resolution :: (B.C f (Kernel.Prelude.Maybe Domain.Types.OccupancyAlert.AlertResolution)),
    resolvedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Domain.Types.OccupancyAlert.AlertStatus),
    ticketPlaceId :: (B.C f Kernel.Prelude.Text),
    unpaidEstimate :: (B.C f Kernel.Prelude.Int),
    violationId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OccupancyAlertT where
  data PrimaryKey OccupancyAlertT f = OccupancyAlertId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OccupancyAlertId . id

type OccupancyAlert = OccupancyAlertT Identity

$(enableKVPG (''OccupancyAlertT) [('id)] [[('ticketPlaceId)]])

$(mkTableInstances (''OccupancyAlertT) "occupancy_alert")
