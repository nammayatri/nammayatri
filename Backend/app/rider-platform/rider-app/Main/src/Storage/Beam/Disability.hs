{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Disability where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data DisabilityT f = DisabilityT
  { id :: B.C f Text,
    tag :: B.C f Text,
    description :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table DisabilityT where
  data PrimaryKey DisabilityT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Disability = DisabilityT Identity

$(enableKVPG ''DisabilityT ['id] [])

$(mkTableInstances ''DisabilityT "disability")
