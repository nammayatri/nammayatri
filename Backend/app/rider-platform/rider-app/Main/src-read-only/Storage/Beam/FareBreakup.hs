{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FareBreakup where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Domain.Types.FareBreakup
import qualified Database.Beam as B



data FareBreakupT f
    = FareBreakupT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                    description :: (B.C f Kernel.Prelude.Text),
                    bookingId :: (B.C f Kernel.Prelude.Text),
                    entityType :: (B.C f Domain.Types.FareBreakup.FareBreakupEntityType),
                    id :: (B.C f Kernel.Prelude.Text)}
    deriving (Generic, B.Beamable)
instance B.Table FareBreakupT
    where data PrimaryKey FareBreakupT f = FareBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FareBreakupId . id
type FareBreakup = FareBreakupT Identity

$(enableKVPG (''FareBreakupT) [('id)] [[('bookingId)]])

$(mkTableInstances (''FareBreakupT) "fare_breakup")

