{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CancellationDuesDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.CancellationDuesDetails
import qualified Database.Beam as B



data CancellationDuesDetailsT f
    = CancellationDuesDetailsT {cancellationAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                                id :: (B.C f Kernel.Prelude.Text),
                                paymentStatus :: (B.C f Domain.Types.CancellationDuesDetails.CancellationDuesPaymentStatus),
                                rideId :: (B.C f Kernel.Prelude.Text),
                                riderId :: (B.C f Kernel.Prelude.Text),
                                updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                                merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table CancellationDuesDetailsT
    where data PrimaryKey CancellationDuesDetailsT f = CancellationDuesDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CancellationDuesDetailsId . id
type CancellationDuesDetails = CancellationDuesDetailsT Identity

$(enableKVPG (''CancellationDuesDetailsT) [('id)] [[('rideId)]])

$(mkTableInstances (''CancellationDuesDetailsT) "cancellation_dues_details")

