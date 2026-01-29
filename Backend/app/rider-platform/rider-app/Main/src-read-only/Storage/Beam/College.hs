{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.College where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CollegeT f = CollegeT
  { collegeAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    collegeName :: (B.C f Kernel.Prelude.Text),
    contactName :: (B.C f Kernel.Prelude.Text),
    contactPhoneNumberEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    contactPhoneNumberHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
    contactRole :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table CollegeT where
  data PrimaryKey CollegeT f = CollegeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CollegeId . id

type College = CollegeT Identity

$(enableKVPG (''CollegeT) [('id)] [[('collegeName)]])

$(mkTableInstances (''CollegeT) "college")
