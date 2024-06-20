{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareBreakup where

import qualified Database.Beam as B
import qualified Domain.Types.FareBreakup
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareBreakupT f = FareBreakupT
  { id :: B.C f Kernel.Prelude.Text,
    description :: B.C f Kernel.Prelude.Text,
    amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    entityType :: B.C f Domain.Types.FareBreakup.FareBreakupEntityType,
    bookingId :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table FareBreakupT where
  data PrimaryKey FareBreakupT f = FareBreakupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareBreakupId . id

type FareBreakup = FareBreakupT Identity

$(enableKVPG ''FareBreakupT ['id] [['bookingId]])

$(mkTableInstances ''FareBreakupT "fare_breakup")
