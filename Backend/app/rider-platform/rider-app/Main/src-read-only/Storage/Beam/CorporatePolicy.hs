{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CorporatePolicy where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CorporatePolicyT f = CorporatePolicyT
  { id :: B.C f Kernel.Prelude.Text,
    corporateEntityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    policyType :: B.C f Kernel.Prelude.Text,
    maxFarePerTrip :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    maxMonthlyBudgetPerEmployee :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    allowedServiceTiers :: B.C f Kernel.Prelude.Text,
    requiresApproval :: B.C f Kernel.Prelude.Bool,
    nightShiftSafetyEnabled :: B.C f Kernel.Prelude.Bool,
    womenSafetyRulesEnabled :: B.C f Kernel.Prelude.Bool,
    surgeCap :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    isActive :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CorporatePolicyT where
  data PrimaryKey CorporatePolicyT f = CorporatePolicyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CorporatePolicyId . id

type CorporatePolicy = CorporatePolicyT Identity

$(enableKVPG ''CorporatePolicyT ['id] [['corporateEntityId]])

$(mkTableInstances ''CorporatePolicyT "corporate_policy")
