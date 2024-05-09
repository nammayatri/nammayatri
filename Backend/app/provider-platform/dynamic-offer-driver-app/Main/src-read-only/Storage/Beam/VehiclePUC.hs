{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VehiclePUC where

import qualified Database.Beam as B
import qualified Domain.Types.IdfyVerification
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VehiclePUCT f = VehiclePUCT
  { documentImageId :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    pucExpiry :: B.C f Kernel.Prelude.UTCTime,
    rcId :: B.C f Kernel.Prelude.Text,
    verificationStatus :: B.C f Domain.Types.IdfyVerification.VerificationStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VehiclePUCT where
  data PrimaryKey VehiclePUCT f = VehiclePUCId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VehiclePUCId . id

type VehiclePUC = VehiclePUCT Identity

$(enableKVPG ''VehiclePUCT ['id] [])

$(mkTableInstances ''VehiclePUCT "vehicle_puc")
