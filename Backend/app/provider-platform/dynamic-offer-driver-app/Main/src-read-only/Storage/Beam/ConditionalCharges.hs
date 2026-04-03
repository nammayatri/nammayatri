{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.ConditionalCharges where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Domain.Types.Extra.ConditionalCharges
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Database.Beam as B



data ConditionalChargesT f
    = ConditionalChargesT {cgstPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                           charge :: (B.C f Kernel.Types.Common.HighPrecMoney),
                           chargeCategory :: (B.C f Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories),
                           farePolicyId :: (B.C f Data.Text.Text),
                           sgstPercentage :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table ConditionalChargesT
    where data PrimaryKey ConditionalChargesT f = ConditionalChargesId (B.C f Domain.Types.Extra.ConditionalCharges.ConditionalChargesCategories) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = ConditionalChargesId <$> chargeCategory <*> farePolicyId
type ConditionalCharges = ConditionalChargesT Identity

$(enableKVPG (''ConditionalChargesT) [('chargeCategory), ('farePolicyId)] [])

$(mkTableInstances (''ConditionalChargesT) "conditional_charges")

