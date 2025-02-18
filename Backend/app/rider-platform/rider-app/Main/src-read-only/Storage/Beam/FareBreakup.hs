{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareBreakup where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FareBreakup
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareBreakupT f = FareBreakupT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    description :: B.C f Kernel.Prelude.Text,
    bookingId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.FareBreakup.FareBreakupEntityType,
    id :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table FareBreakupT where
  data PrimaryKey FareBreakupT f = FareBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareBreakupId . id

type FareBreakup = FareBreakupT Identity

$(enableKVPG ''FareBreakupT ['id] [['bookingId]])

$(mkTableInstances ''FareBreakupT "fare_breakup")
