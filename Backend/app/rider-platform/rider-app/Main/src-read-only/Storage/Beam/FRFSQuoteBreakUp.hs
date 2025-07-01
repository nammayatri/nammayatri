{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSQuoteBreakUp where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteBreakUp
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSQuoteBreakUpT f = FRFSQuoteBreakUpT
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    description :: (B.C f Domain.Types.FRFSQuoteBreakUp.QuoteBreakupDescription),
    id :: (B.C f Kernel.Prelude.Text),
    quoteId :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSQuoteBreakUpT where
  data PrimaryKey FRFSQuoteBreakUpT f = FRFSQuoteBreakUpId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSQuoteBreakUpId . id

type FRFSQuoteBreakUp = FRFSQuoteBreakUpT Identity

$(enableKVPG (''FRFSQuoteBreakUpT) [('id)] [[('quoteId)]])

$(mkTableInstances (''FRFSQuoteBreakUpT) "frfs_quote_break_up")
