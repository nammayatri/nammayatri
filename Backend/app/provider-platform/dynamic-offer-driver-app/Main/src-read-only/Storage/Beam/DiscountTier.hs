{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DiscountTier where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DiscountTier
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DiscountTierT f = DiscountTierT
  { discountId :: (B.C f Kernel.Prelude.Text),
    discountValue :: (B.C f Kernel.Prelude.Double),
    discountValueType :: (B.C f Domain.Types.DiscountTier.DiscountValueType),
    id :: (B.C f Kernel.Prelude.Text),
    thresholdValue :: (B.C f Kernel.Prelude.Double),
    tierOrder :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DiscountTierT where
  data PrimaryKey DiscountTierT f = DiscountTierId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DiscountTierId . id

type DiscountTier = DiscountTierT Identity

$(enableKVPG (''DiscountTierT) [('id)] [[('discountId)]])

$(mkTableInstances (''DiscountTierT) "discount_tier")
