{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverGroupInsurance where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverGroupInsurance
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverGroupInsuranceT f = DriverGroupInsuranceT
  { age :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    dob :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
    driverId :: (B.C f Kernel.Prelude.Text),
    enabledAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    fullName :: (B.C f Kernel.Prelude.Text),
    gender :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverGroupInsurance.DriverGroupInsuranceGender)),
    id :: (B.C f Kernel.Prelude.Text),
    insuranceType :: (B.C f Domain.Types.DriverGroupInsurance.DriverGroupInsuranceType),
    lastExportedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    mobile :: (B.C f Kernel.Prelude.Text),
    nomineeDob :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
    nomineeName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    nomineeRelationship :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    secondBotCheckAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Domain.Types.DriverGroupInsurance.DriverGroupInsuranceStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverGroupInsuranceT where
  data PrimaryKey DriverGroupInsuranceT f = DriverGroupInsuranceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverGroupInsuranceId . id

type DriverGroupInsurance = DriverGroupInsuranceT Identity

$(enableKVPG (''DriverGroupInsuranceT) [('id)] [[('driverId)]])

$(mkTableInstances (''DriverGroupInsuranceT) "driver_group_insurance")
