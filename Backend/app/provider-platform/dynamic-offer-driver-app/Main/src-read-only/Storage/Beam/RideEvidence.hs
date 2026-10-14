{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideEvidence where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Ride
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RideEvidenceT f = RideEvidenceT
  { description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    driverId :: (B.C f Kernel.Prelude.Text),
    hiddenAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    hiddenBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    hiddenReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    mediaFileId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    rideId :: (B.C f Kernel.Prelude.Text),
    rideStatus :: (B.C f Domain.Types.Ride.RideStatus),
    thumbnailMediaFileId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideEvidenceT where
  data PrimaryKey RideEvidenceT f = RideEvidenceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideEvidenceId . id

type RideEvidence = RideEvidenceT Identity

$(enableKVPG (''RideEvidenceT) [('id)] [[('mediaFileId)], [('rideId)]])

$(mkTableInstances (''RideEvidenceT) "ride_evidence")
