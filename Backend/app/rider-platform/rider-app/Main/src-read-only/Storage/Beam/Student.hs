{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Student where

import qualified Data.Aeson
import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Student
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data StudentT f = StudentT
  { address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    age :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Data.Time.UTCTime),
    graduationDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime)),
    guardianName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    idCardPicture :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    name :: (B.C f Kernel.Prelude.Text),
    numberOfStages :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    organizationId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    remark :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    routePairs :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    studentClass :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Data.Time.UTCTime),
    verificationDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime)),
    verificationStatus :: (B.C f Domain.Types.Student.VerificationStatus),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table StudentT where
  data PrimaryKey StudentT f = StudentId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StudentId . id

type Student = StudentT Identity

$(enableKVPG (''StudentT) [('id)] [[('organizationId)], [('personId)]])

$(mkTableInstances (''StudentT) "student")
