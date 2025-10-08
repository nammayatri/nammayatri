{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Sos where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Sos
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SosT f = SosT
  { flow :: B.C f Domain.Types.Sos.SosType,
    id :: B.C f Kernel.Prelude.Text,
    mediaFiles :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    personId :: B.C f Kernel.Prelude.Text,
    rideId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.Sos.SosStatus,
    ticketId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SosT where
  data PrimaryKey SosT f = SosId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SosId . id

type Sos = SosT Identity

$(enableKVPG ''SosT ['id] [['rideId]])

$(mkTableInstances ''SosT "sos")
