{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VehicleInfo where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data VehicleInfoT f
    = VehicleInfoT {answer :: (B.C f Kernel.Prelude.Text), question :: (B.C f Kernel.Prelude.Text), questionId :: (B.C f Kernel.Prelude.Text), rcId :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table VehicleInfoT
    where data PrimaryKey VehicleInfoT f = VehicleInfoId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = VehicleInfoId <$> questionId <*> rcId
type VehicleInfo = VehicleInfoT Identity

$(enableKVPG (''VehicleInfoT) [('questionId), ('rcId)] [])

$(mkTableInstances (''VehicleInfoT) "vehicle_info")

