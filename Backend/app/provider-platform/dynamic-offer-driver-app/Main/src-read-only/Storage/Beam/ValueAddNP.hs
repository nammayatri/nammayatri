{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ValueAddNP where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ValueAddNPT f = ValueAddNPT {enabled :: B.C f Kernel.Prelude.Bool, subscriberId :: B.C f Kernel.Prelude.Text, createdAt :: B.C f Kernel.Prelude.UTCTime, updatedAt :: B.C f Kernel.Prelude.UTCTime}
  deriving (Generic, B.Beamable)

instance B.Table ValueAddNPT where
  data PrimaryKey ValueAddNPT f = ValueAddNPId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ValueAddNPId . subscriberId

type ValueAddNP = ValueAddNPT Identity

$(enableKVPG ''ValueAddNPT ['subscriberId] [])

$(mkTableInstances ''ValueAddNPT "value_add_np")
