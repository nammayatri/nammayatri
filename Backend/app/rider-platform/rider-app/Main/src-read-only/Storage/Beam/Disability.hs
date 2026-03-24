{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.Disability where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DisabilityT f = DisabilityT {description :: (B.C f Kernel.Prelude.Text), id :: (B.C f Kernel.Prelude.Text), tag :: (B.C f Kernel.Prelude.Text)} deriving (Generic, B.Beamable)
instance B.Table DisabilityT
    where data PrimaryKey DisabilityT f = DisabilityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DisabilityId . id
type Disability = DisabilityT Identity

$(enableKVPG (''DisabilityT) [('id)] [])

$(mkTableInstances (''DisabilityT) "disability")

