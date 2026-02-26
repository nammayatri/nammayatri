{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MorthRCVerification where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MorthRCVerificationT f = MorthRCVerificationT
  { id :: B.C f Kernel.Prelude.Text,
    driverId :: B.C f Kernel.Prelude.Text,
    rcNumber :: B.C f Kernel.Prelude.Text,
    success :: B.C f Kernel.Prelude.Bool,
    rcValidity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rcValidUpto :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    message :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    statusCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MorthRCVerificationT where
  data PrimaryKey MorthRCVerificationT f = MorthRCVerificationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MorthRCVerificationId . id

type MorthRCVerification = MorthRCVerificationT Identity

$(enableKVPG ''MorthRCVerificationT ['id] [['driverId], ['rcNumber]])

$(mkTableInstances ''MorthRCVerificationT "morth_rc_verification")
