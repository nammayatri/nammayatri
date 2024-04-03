{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehiclePermit where

import qualified Database.Beam as B
import qualified Domain.Types.IdfyVerification
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehiclePermitT f = VehiclePermitT
  { documentImageId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    issueDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    nameOfPermitHolder :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    permitExpiry :: (B.C f Kernel.Prelude.UTCTime),
    permitNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    permitNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    purposeOfJourney :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rcId :: (B.C f Kernel.Prelude.Text),
    regionCovered :: (B.C f Kernel.Prelude.Text),
    verificationStatus :: (B.C f Domain.Types.IdfyVerification.VerificationStatus),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table VehiclePermitT where
  data PrimaryKey VehiclePermitT f = VehiclePermitId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehiclePermitId . id

type VehiclePermit = VehiclePermitT Identity

$(enableKVPG (''VehiclePermitT) [('id)] [])

$(mkTableInstances (''VehiclePermitT) "vehicle_permit")
