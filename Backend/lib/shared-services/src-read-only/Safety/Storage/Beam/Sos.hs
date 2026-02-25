{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Safety.Storage.Beam.Sos where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Safety.Domain.Types.Sos

data SosT f = SosT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    entityType :: B.C f (Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosEntityType),
    externalReferenceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    externalReferenceStatus :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    externalStatusHistory :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    flow :: B.C f Safety.Domain.Types.Sos.SosType,
    id :: B.C f Kernel.Prelude.Text,
    mediaFiles :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    personId :: B.C f Kernel.Prelude.Text,
    rideId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    sosState :: B.C f (Kernel.Prelude.Maybe Safety.Domain.Types.Sos.SosState),
    status :: B.C f Safety.Domain.Types.Sos.SosStatus,
    ticketId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    trackingExpiresAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SosT where
  data PrimaryKey SosT f = SosId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SosId . id

type Sos = SosT Identity

$(enableKVPG ''SosT ['id] [])

$(mkTableInstancesGenericSchema ''SosT "sos")
