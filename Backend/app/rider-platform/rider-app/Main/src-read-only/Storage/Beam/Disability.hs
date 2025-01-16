{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Disability where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DisabilityT f = DisabilityT {description :: B.C f Kernel.Prelude.Text, id :: B.C f Kernel.Prelude.Text, tag :: B.C f Kernel.Prelude.Text} deriving (Generic, B.Beamable)

instance B.Table DisabilityT where
  data PrimaryKey DisabilityT f = DisabilityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DisabilityId . id

type Disability = DisabilityT Identity

$(enableKVPG ''DisabilityT ['id] [])

$(mkTableInstances ''DisabilityT "disability")
