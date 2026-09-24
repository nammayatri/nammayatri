{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicyRentalDetailsDistanceBuffers where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Utils.Time
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsDistanceBuffersT f = FarePolicyRentalDetailsDistanceBuffersT
  { bufferKms :: (B.C f Kernel.Prelude.Int),
    bufferMeters :: (B.C f Kernel.Prelude.Int),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    rideDuration :: (B.C f Kernel.Utils.Time.Seconds)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsDistanceBuffersT where
  data PrimaryKey FarePolicyRentalDetailsDistanceBuffersT f = FarePolicyRentalDetailsDistanceBuffersId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FarePolicyRentalDetailsDistanceBuffersId . farePolicyId

type FarePolicyRentalDetailsDistanceBuffers = FarePolicyRentalDetailsDistanceBuffersT Identity

$(enableKVPG (''FarePolicyRentalDetailsDistanceBuffersT) [('farePolicyId)] [])

$(mkTableInstances (''FarePolicyRentalDetailsDistanceBuffersT) "fare_policy_rental_details_distance_buffers")
