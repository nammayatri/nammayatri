{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ConditionalCharges where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.ConditionalCharges
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data ConditionalChargesT f = ConditionalChargesT
  { cgstPercentage :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    charge :: B.C f Kernel.Types.Common.HighPrecMoney,
    chargeCategory :: B.C f Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories,
    farePolicyId :: B.C f Data.Text.Text,
    sgstPercentage :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ConditionalChargesT where
  data PrimaryKey ConditionalChargesT f = ConditionalChargesId (B.C f Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = ConditionalChargesId <$> chargeCategory <*> farePolicyId

type ConditionalCharges = ConditionalChargesT Identity

$(enableKVPG ''ConditionalChargesT ['chargeCategory, 'farePolicyId] [])

$(mkTableInstances ''ConditionalChargesT "conditional_charges")
