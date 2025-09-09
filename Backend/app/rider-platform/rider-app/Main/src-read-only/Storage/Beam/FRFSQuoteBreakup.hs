{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSQuoteBreakup where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteCategorySpec
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSQuoteBreakupT f = FRFSQuoteBreakupT
  { id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    quoteCategoryId :: (B.C f Kernel.Prelude.Text),
    quoteId :: (B.C f Kernel.Prelude.Text),
    tag :: (B.C f Domain.Types.FRFSQuoteCategorySpec.FRFSCategoryTag),
    value :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSQuoteBreakupT where
  data PrimaryKey FRFSQuoteBreakupT f = FRFSQuoteBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSQuoteBreakupId . id

type FRFSQuoteBreakup = FRFSQuoteBreakupT Identity

$(enableKVPG (''FRFSQuoteBreakupT) [('id)] [[('quoteCategoryId)], [('quoteId)]])

$(mkTableInstances (''FRFSQuoteBreakupT) "frfs_quote_breakup")
