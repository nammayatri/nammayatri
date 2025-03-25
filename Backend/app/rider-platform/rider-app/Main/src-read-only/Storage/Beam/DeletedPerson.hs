{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DeletedPerson where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data DeletedPersonT f = DeletedPersonT
  { clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    deviceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    reasonToDelete :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DeletedPersonT where
  data PrimaryKey DeletedPersonT f = DeletedPersonId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DeletedPersonId . personId

type DeletedPerson = DeletedPersonT Identity

$(enableKVPG ''DeletedPersonT ['personId] [])

$(mkTableInstances ''DeletedPersonT "deleted_person")
