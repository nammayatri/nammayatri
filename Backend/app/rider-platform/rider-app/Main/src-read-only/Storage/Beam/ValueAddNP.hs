{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.ValueAddNP where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data ValueAddNPT f
    = ValueAddNPT {enabled :: (B.C f Kernel.Prelude.Bool), subscriberId :: (B.C f Kernel.Prelude.Text), createdAt :: (B.C f Kernel.Prelude.UTCTime), updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ValueAddNPT
    where data PrimaryKey ValueAddNPT f = ValueAddNPId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = ValueAddNPId . subscriberId
type ValueAddNP = ValueAddNPT Identity

$(enableKVPG (''ValueAddNPT) [('subscriberId)] [])

$(mkTableInstances (''ValueAddNPT) "value_add_np")

