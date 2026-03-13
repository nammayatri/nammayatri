{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateEmployee where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateEmployeeT f = CorporateEmployeeT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    employeeCode :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    email :: B.C f Kernel.Prelude.Text,
    phone :: B.C f Kernel.Prelude.Text,
    department :: B.C f Kernel.Prelude.Text,
    costCenter :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gender :: B.C f Kernel.Prelude.Text,
    officeLocationId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    defaultPickupLat :: B.C f Kernel.Prelude.Double,
    defaultPickupLon :: B.C f Kernel.Prelude.Double,
    defaultPickupAddress :: B.C f Kernel.Prelude.Text,
    reportingManagerEmail :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Kernel.Prelude.Text,
    linkedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateEmployeeT where
  data PrimaryKey CorporateEmployeeT f = CorporateEmployeeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateEmployeeId . id

type CorporateEmployee = CorporateEmployeeT Identity

$(enableKVPG ''CorporateEmployeeT ['id] [['corporateEntityId], ['personId]])

$(mkTableInstances ''CorporateEmployeeT "corporate_employee")
