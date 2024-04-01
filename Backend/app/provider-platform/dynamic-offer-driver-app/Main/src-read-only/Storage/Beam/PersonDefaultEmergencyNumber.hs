{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonDefaultEmergencyNumber where

import qualified Database.Beam as B
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonDefaultEmergencyNumberT f = PersonDefaultEmergencyNumberT
  { merchantId :: B.C f Kernel.Prelude.Text,
    mobileCountryCode :: B.C f Kernel.Prelude.Text,
    mobileNumberEncrypted :: B.C f Kernel.Prelude.Text,
    mobileNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    name :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonDefaultEmergencyNumberT where
  data PrimaryKey PersonDefaultEmergencyNumberT f
    = PersonDefaultEmergencyNumberId (B.C f Kernel.Prelude.Text) (B.C f Kernel.External.Encryption.DbHash) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PersonDefaultEmergencyNumberId <$> mobileCountryCode <*> mobileNumberHash <*> personId

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberT Identity

$(enableKVPG ''PersonDefaultEmergencyNumberT ['mobileCountryCode, 'mobileNumberHash, 'personId] [])

$(mkTableInstances ''PersonDefaultEmergencyNumberT "person_default_emergency_number")
