{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IncentiveJourney where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data IncentiveJourneyT f = IncentiveJourneyT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    driverTag :: (B.C f Kernel.Prelude.Text),
    enabled :: (B.C f Kernel.Prelude.Bool),
    endDate :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    journeyType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.IncentiveJourney.IncentiveJourneyType)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    startDate :: (B.C f Kernel.Prelude.UTCTime),
    timeBounds :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory))
  }
  deriving (Generic, B.Beamable)

instance B.Table IncentiveJourneyT where
  data PrimaryKey IncentiveJourneyT f = IncentiveJourneyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IncentiveJourneyId . id

type IncentiveJourney = IncentiveJourneyT Identity

$(enableKVPG (''IncentiveJourneyT) [('id)] [])

$(mkTableInstances (''IncentiveJourneyT) "incentive_journey")
