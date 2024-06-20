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
  { personId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    mobileNumberEncrypted :: B.C f Kernel.Prelude.Text,
    mobileNumberHash :: B.C f Kernel.External.Encryption.DbHash,
    mobileCountryCode :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    contactPersonId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    enableForFollowing :: B.C f Kernel.Prelude.Bool,
    enableForShareRide :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    priority :: B.C f Kernel.Prelude.Int
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonDefaultEmergencyNumberT where
  data PrimaryKey PersonDefaultEmergencyNumberT f = PersonDefaultEmergencyNumberId (B.C f Kernel.Prelude.Text) (B.C f Kernel.External.Encryption.DbHash) deriving (Generic, B.Beamable)
  primaryKey = PersonDefaultEmergencyNumberId <$> personId <*> mobileNumberHash

type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberT Identity

$(enableKVPG ''PersonDefaultEmergencyNumberT ['personId, 'mobileNumberHash] [['personId], ['mobileNumberHash]])

$(mkTableInstances ''PersonDefaultEmergencyNumberT "person_default_emergency_number")
