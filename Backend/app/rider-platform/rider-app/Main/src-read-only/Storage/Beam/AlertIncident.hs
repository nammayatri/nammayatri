{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AlertIncident where

import qualified Database.Beam as B
import qualified Domain.Types.AlertIncident
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data AlertIncidentT f = AlertIncidentT
  { alertGroup :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    alertName :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    downtimeSeconds :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    externalURL :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    firingTime :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    rawPayload :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    receiver :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    resolvedTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    serviceName :: B.C f Kernel.Prelude.Text,
    severity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.AlertIncident.IncidentStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AlertIncidentT where
  data PrimaryKey AlertIncidentT f = AlertIncidentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AlertIncidentId . id

type AlertIncident = AlertIncidentT Identity

$(enableKVPG ''AlertIncidentT ['id] [])

$(mkTableInstances ''AlertIncidentT "alert_incident")
