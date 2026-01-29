{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Student where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Student
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data StudentT f = StudentT
  { collegeId :: (B.C f Kernel.Prelude.Text),
    collegeName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    destinationStop :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    graduationDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    guardianName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    intermediateStops :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    sourceStop :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    studentAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    studentAge :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    studentClass :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    studentName :: (B.C f Kernel.Prelude.Text),
    studentPicture :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    verificationDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    verificationStatus :: (B.C f Domain.Types.Student.VerificationStatus)
  }
  deriving (Generic, B.Beamable)

instance B.Table StudentT where
  data PrimaryKey StudentT f = StudentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StudentId . id

type Student = StudentT Identity

$(enableKVPG (''StudentT) [('id)] [])

$(mkTableInstances (''StudentT) "student")
