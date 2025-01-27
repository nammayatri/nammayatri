{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehicleInsurance where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data VehicleInsuranceT f = VehicleInsuranceT
  { documentImageId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    insuredName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    issueDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    limitsOfLiability :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    policyExpiry :: B.C f Kernel.Prelude.UTCTime,
    policyNumberEncrypted :: B.C f Kernel.Prelude.Text,
    policyNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    policyProvider :: B.C f Kernel.Prelude.Text,
    rcId :: B.C f Kernel.Prelude.Text,
    rejectReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    verificationStatus :: B.C f Kernel.Types.Documents.VerificationStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehicleInsuranceT where
  data PrimaryKey VehicleInsuranceT f = VehicleInsuranceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehicleInsuranceId . id

type VehicleInsurance = VehicleInsuranceT Identity

$(enableKVPG ''VehicleInsuranceT ['id] [['documentImageId]])

$(mkTableInstances ''VehicleInsuranceT "vehicle_insurance")
