{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSSavedPassenger where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Person
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSSavedPassengerT f = FRFSSavedPassengerT
  { age :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    gender :: (B.C f Domain.Types.Person.Gender),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    riderId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSSavedPassengerT where
  data PrimaryKey FRFSSavedPassengerT f = FRFSSavedPassengerId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSSavedPassengerId . id

type FRFSSavedPassenger = FRFSSavedPassengerT Identity

$(enableKVPG (''FRFSSavedPassengerT) [('id)] [[('riderId)]])

$(mkTableInstances (''FRFSSavedPassengerT) "frfs_saved_passenger")
