{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PassType where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PassTypeT f = PassTypeT
  { catchline :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    order :: B.C f Kernel.Prelude.Int,
    passCategoryId :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PassTypeT where
  data PrimaryKey PassTypeT f = PassTypeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassTypeId . id

type PassType = PassTypeT Identity

$(enableKVPG ''PassTypeT ['id] [])

$(mkTableInstances ''PassTypeT "pass_type")
