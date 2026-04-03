{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CancellationCharges where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data CancellationChargesT f
    = CancellationChargesT {cancellationCharges :: (B.C f Kernel.Types.Common.HighPrecMoney),
                            currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                            driverId :: (B.C f Kernel.Prelude.Text),
                            id :: (B.C f Kernel.Prelude.Text),
                            rideId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table CancellationChargesT
    where data PrimaryKey CancellationChargesT f = CancellationChargesId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CancellationChargesId . id
type CancellationCharges = CancellationChargesT Identity

$(enableKVPG (''CancellationChargesT) [('id)] [])

$(mkTableInstances (''CancellationChargesT) "cancellation_charges")

