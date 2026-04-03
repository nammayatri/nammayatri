{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.LmsCertificate where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data LmsCertificateT f
    = LmsCertificateT {driverId :: (B.C f Kernel.Prelude.Text),
                       id :: (B.C f Kernel.Prelude.Text),
                       moduleCompletionId :: (B.C f Kernel.Prelude.Text),
                       moduleId :: (B.C f Kernel.Prelude.Text),
                       createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table LmsCertificateT
    where data PrimaryKey LmsCertificateT f = LmsCertificateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = LmsCertificateId . id
type LmsCertificate = LmsCertificateT Identity

$(enableKVPG (''LmsCertificateT) [('id)] [[('moduleCompletionId)]])

$(mkTableInstances (''LmsCertificateT) "lms_certificate")

