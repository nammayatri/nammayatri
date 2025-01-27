{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverSSN where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data DriverSSNT f = DriverSSNT
  { driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    rejectReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ssnEncrypted :: B.C f Kernel.Prelude.Text,
    ssnHash :: B.C f Kernel.External.Encryption.DbHash,
    verificationStatus :: B.C f Kernel.Types.Documents.VerificationStatus
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverSSNT where
  data PrimaryKey DriverSSNT f = DriverSSNId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverSSNId . id

type DriverSSN = DriverSSNT Identity

$(enableKVPG ''DriverSSNT ['id] [['driverId], ['ssnHash]])

$(mkTableInstances ''DriverSSNT "driver_ssn")
