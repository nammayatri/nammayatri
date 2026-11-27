{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Domain.Types.PermitApplicationEvent where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Permit.Domain.Types.PermitApplication
import qualified Tools.Beam.UtilsTH

data PermitApplicationEvent = PermitApplicationEvent
  { action :: Kernel.Prelude.Text,
    actorId :: Kernel.Prelude.Text,
    actorName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Lib.Permit.Domain.Types.PermitApplicationEvent.PermitActorType,
    applicationId :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicationEvent.PermitApplicationEvent,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PermitActorType = APPLICANT | OFFICIAL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PermitActorType))
