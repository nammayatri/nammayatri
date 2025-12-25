{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Toll where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.ComputeIntersection
import Tools.Beam.UtilsTH

data TollT f = TollT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    isAutoRickshawAllowed :: B.C f Kernel.Prelude.Bool,
    isTwoWheelerAllowed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    name :: B.C f Kernel.Prelude.Text,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    price :: B.C f Kernel.Types.Common.HighPrecMoney,
    tollEndGates :: B.C f [Kernel.Utils.ComputeIntersection.LineSegment],
    tollStartGates :: B.C f [Kernel.Utils.ComputeIntersection.LineSegment],
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TollT where
  data PrimaryKey TollT f = TollId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TollId . id

type Toll = TollT Identity

$(enableKVPG ''TollT ['id] [])

$(mkTableInstances ''TollT "toll")
