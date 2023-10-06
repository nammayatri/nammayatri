{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Disability where

import qualified Database.Beam as B
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

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

$(TH.enableKVPG ''DisabilityT ['id] [])

$(TH.mkTableInstances ''DisabilityT "disability")
