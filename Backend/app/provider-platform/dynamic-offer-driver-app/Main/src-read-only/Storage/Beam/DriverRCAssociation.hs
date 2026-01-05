{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverRCAssociation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DriverRCAssociationT f = DriverRCAssociationT
  { associatedOn :: B.C f Kernel.Prelude.UTCTime,
    associatedTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    consent :: B.C f Kernel.Prelude.Bool,
    consentTimestamp :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    errorMessage :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isRcActive :: B.C f Kernel.Prelude.Bool,
    rcId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverRCAssociationT where
  data PrimaryKey DriverRCAssociationT f = DriverRCAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverRCAssociationId . id

type DriverRCAssociation = DriverRCAssociationT Identity

$(enableKVPG ''DriverRCAssociationT ['id] [['driverId], ['rcId]])

$(mkTableInstances ''DriverRCAssociationT "driver_rc_association")
