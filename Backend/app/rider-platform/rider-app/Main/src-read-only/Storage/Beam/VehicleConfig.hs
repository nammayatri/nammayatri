{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VehicleConfig where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data VehicleConfigT f
    = VehicleConfigT {becknConfigId :: (B.C f Kernel.Prelude.Text),
                      blackListedSubscribers :: (B.C f [Kernel.Prelude.Text]),
                      buyerFinderFee :: (B.C f Kernel.Prelude.Text),
                      category :: (B.C f Kernel.Prelude.Text),
                      id :: (B.C f Kernel.Prelude.Text),
                      merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VehicleConfigT
    where data PrimaryKey VehicleConfigT f = VehicleConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = VehicleConfigId . id
type VehicleConfig = VehicleConfigT Identity

$(enableKVPG (''VehicleConfigT) [('id)] [])

$(mkTableInstances (''VehicleConfigT) "beckn_vehicle_config")

