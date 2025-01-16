{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.LmsCertificate where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data LmsCertificateT f = LmsCertificateT
  { driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    moduleCompletionId :: B.C f Kernel.Prelude.Text,
    moduleId :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table LmsCertificateT where
  data PrimaryKey LmsCertificateT f = LmsCertificateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LmsCertificateId . id

type LmsCertificate = LmsCertificateT Identity

$(enableKVPG ''LmsCertificateT ['id] [['moduleCompletionId]])

$(mkTableInstances ''LmsCertificateT "lms_certificate")
