{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AlertIncidentTimeline where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AlertIncidentTimelineT f = AlertIncidentTimelineT
  { attachments :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    content :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    eventTime :: (B.C f Kernel.Prelude.UTCTime),
    eventType :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    incidentId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AlertIncidentTimelineT where
  data PrimaryKey AlertIncidentTimelineT f = AlertIncidentTimelineId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AlertIncidentTimelineId . id

type AlertIncidentTimeline = AlertIncidentTimelineT Identity

$(enableKVPG (''AlertIncidentTimelineT) [('id)] [])

$(mkTableInstances (''AlertIncidentTimelineT) "alert_incident_timeline")
