{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DiscountTierTranslation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DiscountTierTranslationT f = DiscountTierTranslationT
  { description :: (B.C f Kernel.Prelude.Text),
    language :: (B.C f Kernel.External.Types.Language),
    name :: (B.C f Kernel.Prelude.Text),
    tierId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DiscountTierTranslationT where
  data PrimaryKey DiscountTierTranslationT f = DiscountTierTranslationId (B.C f Kernel.External.Types.Language) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DiscountTierTranslationId <$> language <*> tierId

type DiscountTierTranslation = DiscountTierTranslationT Identity

$(enableKVPG (''DiscountTierTranslationT) [('language), ('tierId)] [])

$(mkTableInstances (''DiscountTierTranslationT) "discount_tier_translation")
