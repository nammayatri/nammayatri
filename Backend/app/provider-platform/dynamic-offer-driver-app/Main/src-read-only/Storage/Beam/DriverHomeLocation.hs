{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverHomeLocation where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data DriverHomeLocationT f
    = DriverHomeLocationT {address :: (B.C f Kernel.Prelude.Text),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           driverId :: (B.C f Kernel.Prelude.Text),
                           id :: (B.C f Kernel.Prelude.Text),
                           lat :: (B.C f Kernel.Prelude.Double),
                           lon :: (B.C f Kernel.Prelude.Double),
                           tag :: (B.C f Kernel.Prelude.Text),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table DriverHomeLocationT
    where data PrimaryKey DriverHomeLocationT f = DriverHomeLocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverHomeLocationId . id
type DriverHomeLocation = DriverHomeLocationT Identity

$(enableKVPG (''DriverHomeLocationT) [('id)] [[('driverId)]])

$(mkTableInstancesWithTModifier (''DriverHomeLocationT) "driver_home_location" [("address", "home_address")])

