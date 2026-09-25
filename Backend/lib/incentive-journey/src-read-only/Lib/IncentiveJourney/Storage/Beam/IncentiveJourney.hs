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
    enabled :: B.C f Kernel.Prelude.Bool,
    id :: B.C f Kernel.Prelude.Text,
    journeyType :: B.C f (Kernel.Prelude.Maybe Lib.IncentiveJourney.Domain.Types.IncentiveJourney.IncentiveJourneyType),
    maxWaiveOffCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
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
