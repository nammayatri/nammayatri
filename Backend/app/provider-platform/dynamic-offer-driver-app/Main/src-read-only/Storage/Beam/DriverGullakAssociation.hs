{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverGullakAssociation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DriverGullakAssociationT f
    = DriverGullakAssociationT {driverId :: (B.C f Kernel.Prelude.Text),
                                gullakToken :: (B.C f Kernel.Prelude.Text),
                                merchantId :: (B.C f Kernel.Prelude.Text),
                                merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                tokenExpiry :: (B.C f Kernel.Prelude.UTCTime),
                                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverGullakAssociationT
    where data PrimaryKey DriverGullakAssociationT f = DriverGullakAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverGullakAssociationId . driverId
type DriverGullakAssociation = DriverGullakAssociationT Identity

$(enableKVPG (''DriverGullakAssociationT) [('driverId)] [])

$(mkTableInstances (''DriverGullakAssociationT) "driver_gullak_association")

