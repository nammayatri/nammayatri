{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DiscountTranslation where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DiscountTranslationT f = DiscountTranslationT
  { description :: (B.C f Kernel.Prelude.Text),
    discountId :: (B.C f Kernel.Prelude.Text),
    language :: (B.C f Kernel.External.Types.Language),
    name :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DiscountTranslationT where
  data PrimaryKey DiscountTranslationT f = DiscountTranslationId (B.C f Kernel.Prelude.Text) (B.C f Kernel.External.Types.Language) deriving (Generic, B.Beamable)
  primaryKey = DiscountTranslationId <$> discountId <*> language

type DiscountTranslation = DiscountTranslationT Identity

$(enableKVPG (''DiscountTranslationT) [('discountId), ('language)] [])

$(mkTableInstances (''DiscountTranslationT) "discount_translation")
