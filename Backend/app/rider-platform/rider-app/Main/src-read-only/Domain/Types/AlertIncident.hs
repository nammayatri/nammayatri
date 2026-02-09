{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AlertIncident where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AlertIncident = AlertIncident
  { alertGroup :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alertName :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    downtimeSeconds :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    externalURL :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firingTime :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.AlertIncident.AlertIncident,
    isManuallyEntered :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    rawPayload :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    receiver :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    resolvedTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    serviceName :: Kernel.Prelude.Text,
    severity :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.AlertIncident.IncidentStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data IncidentStatus = FIRING | RESOLVED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''IncidentStatus)
