{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverSSN where

import qualified Database.Beam as B
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverSSNT f = DriverSSNT {driverId :: B.C f Kernel.Prelude.Text, id :: B.C f Kernel.Prelude.Text, ssnEncrypted :: B.C f Kernel.Prelude.Text, ssnHash :: B.C f Kernel.External.Encryption.DbHash}
  deriving (Generic, B.Beamable)

instance B.Table DriverSSNT where
  data PrimaryKey DriverSSNT f = DriverSSNId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverSSNId . id

type DriverSSN = DriverSSNT Identity

$(enableKVPG ''DriverSSNT ['id] [['driverId]])

$(mkTableInstances ''DriverSSNT "driver_ssn")
