{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporateEntity where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporateEntityT f = CorporateEntityT
  { id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    partnerOrgId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    name :: B.C f Kernel.Prelude.Text,
    registeredName :: B.C f Kernel.Prelude.Text,
    gstin :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    industry :: B.C f Kernel.Prelude.Text,
    contactPersonName :: B.C f Kernel.Prelude.Text,
    contactEmail :: B.C f Kernel.Prelude.Text,
    contactPhone :: B.C f Kernel.Prelude.Text,
    billingAddress :: B.C f Kernel.Prelude.Text,
    billingModel :: B.C f Kernel.Prelude.Text,
    billingCycleType :: B.C f Kernel.Prelude.Text,
    creditLimit :: B.C f Kernel.Prelude.Double,
    currency :: B.C f Kernel.Prelude.Text,
    status :: B.C f Kernel.Prelude.Text,
    contractStartDate :: B.C f Kernel.Prelude.UTCTime,
    contractEndDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporateEntityT where
  data PrimaryKey CorporateEntityT f = CorporateEntityId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporateEntityId . id

type CorporateEntity = CorporateEntityT Identity

$(enableKVPG ''CorporateEntityT ['id] [['merchantId], ['partnerOrgId]])

$(mkTableInstances ''CorporateEntityT "corporate_entity")
