{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.IncentiveJourney.Storage.Beam.IncentiveJourney where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney

data IncentiveJourneyT f = IncentiveJourneyT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    journeyType :: B.C f Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourneyType,
    name :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IncentiveJourneyT where
  data PrimaryKey IncentiveJourneyT f = IncentiveJourneyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IncentiveJourneyId . id

type IncentiveJourney = IncentiveJourneyT Identity

$(enableKVPG ''IncentiveJourneyT ['id] [])

$(mkTableInstancesGenericSchema ''IncentiveJourneyT "incentive_journey")
