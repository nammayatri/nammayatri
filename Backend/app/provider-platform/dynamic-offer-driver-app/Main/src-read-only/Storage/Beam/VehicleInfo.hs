{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleInfo where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehicleInfoT f = VehicleInfoT {answer :: B.C f Kernel.Prelude.Text, question :: B.C f Kernel.Prelude.Text, questionId :: B.C f Kernel.Prelude.Text, rcId :: B.C f Kernel.Prelude.Text}
  deriving (Generic, B.Beamable)

instance B.Table VehicleInfoT where
  data PrimaryKey VehicleInfoT f = VehicleInfoId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleInfoId <$> questionId <*> rcId

type VehicleInfo = VehicleInfoT Identity

$(enableKVPG ''VehicleInfoT ['questionId, 'rcId] [])

$(mkTableInstances ''VehicleInfoT "vehicle_info")
