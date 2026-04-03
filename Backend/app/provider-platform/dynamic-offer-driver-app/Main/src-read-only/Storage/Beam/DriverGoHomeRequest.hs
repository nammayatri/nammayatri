{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverGoHomeRequest where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DriverGoHomeRequest
import qualified Database.Beam as B



data DriverGoHomeRequestT f
    = DriverGoHomeRequestT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                            driverId :: (B.C f Kernel.Prelude.Text),
                            id :: (B.C f Kernel.Prelude.Text),
                            lat :: (B.C f Kernel.Prelude.Double),
                            lon :: (B.C f Kernel.Prelude.Double),
                            reachedHome :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                            merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                            merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                            numCancellation :: (B.C f Kernel.Prelude.Int),
                            status :: (B.C f Domain.Types.DriverGoHomeRequest.DriverGoHomeRequestStatus),
                            updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverGoHomeRequestT
    where data PrimaryKey DriverGoHomeRequestT f = DriverGoHomeRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverGoHomeRequestId . id
type DriverGoHomeRequest = DriverGoHomeRequestT Identity

$(enableKVPG (''DriverGoHomeRequestT) [('id)] [[('driverId)]])

$(mkTableInstances (''DriverGoHomeRequestT) "driver_go_home_request")

