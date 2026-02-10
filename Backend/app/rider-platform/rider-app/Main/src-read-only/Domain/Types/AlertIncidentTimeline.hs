{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AlertIncidentTimeline where

import Data.Aeson
import qualified Domain.Types.AlertIncident
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AlertIncidentTimeline = AlertIncidentTimeline
  { attachments :: Kernel.Prelude.Maybe Data.Aeson.Value,
    content :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    eventTime :: Kernel.Prelude.UTCTime,
    eventType :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.AlertIncidentTimeline.AlertIncidentTimeline,
    incidentId :: Kernel.Types.Id.Id Domain.Types.AlertIncident.AlertIncident,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
