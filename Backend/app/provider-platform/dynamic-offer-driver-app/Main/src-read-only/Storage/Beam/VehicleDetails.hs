{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VehicleDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Data.Text
import qualified Domain.Types.VehicleVariant
import qualified Database.Beam as B



data VehicleDetailsT f
    = VehicleDetailsT {acAvailable :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                       capacity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                       id :: (B.C f Data.Text.Text),
                       make :: (B.C f Data.Text.Text),
                       model :: (B.C f Data.Text.Text),
                       vehicleVariant :: (B.C f Domain.Types.VehicleVariant.VehicleVariant),
                       year :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int))}
    deriving (Generic, B.Beamable)
instance B.Table VehicleDetailsT
    where data PrimaryKey VehicleDetailsT f = VehicleDetailsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = VehicleDetailsId . id
type VehicleDetails = VehicleDetailsT Identity

$(enableKVPG (''VehicleDetailsT) [('id)] [])

$(mkTableInstances (''VehicleDetailsT) "vehicle_details")

