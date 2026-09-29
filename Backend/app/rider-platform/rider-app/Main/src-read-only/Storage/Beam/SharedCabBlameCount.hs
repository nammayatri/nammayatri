{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedCabBlameCount where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SharedCabBlameCount
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SharedCabBlameCountT f = SharedCabBlameCountT
  { count :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    lastAt :: (B.C f Kernel.Prelude.UTCTime),
    lastBookingId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    subjectId :: (B.C f Kernel.Prelude.Text),
    subjectType :: (B.C f Domain.Types.SharedCabBlameCount.BlameSubjectType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedCabBlameCountT where
  data PrimaryKey SharedCabBlameCountT f = SharedCabBlameCountId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedCabBlameCountId . id

type SharedCabBlameCount = SharedCabBlameCountT Identity

$(enableKVPG (''SharedCabBlameCountT) [('id)] [])

$(mkTableInstances (''SharedCabBlameCountT) "shared_cab_blame_count")
