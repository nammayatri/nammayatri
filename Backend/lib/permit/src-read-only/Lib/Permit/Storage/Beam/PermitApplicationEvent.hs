{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Beam.PermitApplicationEvent where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Permit.Domain.Types.PermitApplicationEvent

data PermitApplicationEventT f = PermitApplicationEventT
  { action :: (B.C f Kernel.Prelude.Text),
    actorId :: (B.C f Kernel.Prelude.Text),
    actorName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    actorType :: (B.C f Lib.Permit.Domain.Types.PermitApplicationEvent.PermitActorType),
    applicationId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    reason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PermitApplicationEventT where
  data PrimaryKey PermitApplicationEventT f = PermitApplicationEventId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PermitApplicationEventId . id

type PermitApplicationEvent = PermitApplicationEventT Identity

$(enableKVPG (''PermitApplicationEventT) [('id)] [[('applicationId)]])

$(mkTableInstancesGenericSchema (''PermitApplicationEventT) "permit_application_event")
