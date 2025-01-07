{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Journey where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data JourneyT f = JourneyT
  { convenienceCost :: B.C f Kernel.Prelude.Int,
    distanceUnit :: B.C f Kernel.Types.Common.DistanceUnit,
    estimatedDistance :: B.C f Kernel.Types.Common.HighPrecDistance,
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    estimatedFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedMaxFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    estimatedMinFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    fare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    id :: B.C f Kernel.Prelude.Text,
    legsDone :: B.C f Kernel.Prelude.Int,
    modes :: B.C f [Domain.Types.Common.TravelMode],
    searchRequestId :: B.C f Kernel.Prelude.Text,
    totalLegs :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table JourneyT where
  data PrimaryKey JourneyT f = JourneyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = JourneyId . id

type Journey = JourneyT Identity

$(enableKVPG ''JourneyT ['id] [['searchRequestId]])

$(mkTableInstances ''JourneyT "journey")
