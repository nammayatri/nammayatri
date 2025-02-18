{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.QuoteBreakup where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data QuoteBreakupT f = QuoteBreakupT
  { id :: B.C f Kernel.Prelude.Text,
    priceCurrency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    priceValue :: B.C f Kernel.Types.Common.HighPrecMoney,
    quoteId :: B.C f Kernel.Prelude.Text,
    title :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table QuoteBreakupT where
  data PrimaryKey QuoteBreakupT f = QuoteBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = QuoteBreakupId . id

type QuoteBreakup = QuoteBreakupT Identity

$(enableKVPG ''QuoteBreakupT ['id] [['quoteId]])

$(mkTableInstances ''QuoteBreakupT "quote_breakup")
