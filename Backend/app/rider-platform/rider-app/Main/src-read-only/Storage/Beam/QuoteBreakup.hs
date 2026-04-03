{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.QuoteBreakup where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Database.Beam as B



data QuoteBreakupT f
    = QuoteBreakupT {id :: (B.C f Kernel.Prelude.Text),
                     priceCurrency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                     priceValue :: (B.C f Kernel.Types.Common.HighPrecMoney),
                     quoteId :: (B.C f Kernel.Prelude.Text),
                     title :: (B.C f Kernel.Prelude.Text),
                     merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                     merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table QuoteBreakupT
    where data PrimaryKey QuoteBreakupT f = QuoteBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = QuoteBreakupId . id
type QuoteBreakup = QuoteBreakupT Identity

$(enableKVPG (''QuoteBreakupT) [('id)] [[('quoteId)]])

$(mkTableInstances (''QuoteBreakupT) "quote_breakup")

