{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonDisability where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonDisabilityT f = PersonDisabilityT
  { createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    disabilityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    tag :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonDisabilityT where
  data PrimaryKey PersonDisabilityT f = PersonDisabilityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonDisabilityId . personId

type PersonDisability = PersonDisabilityT Identity

$(enableKVPG ''PersonDisabilityT ['personId] [])

$(mkTableInstances ''PersonDisabilityT "person_disability")
