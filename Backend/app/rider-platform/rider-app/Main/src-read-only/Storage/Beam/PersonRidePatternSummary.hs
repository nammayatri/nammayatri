{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonRidePatternSummary where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PersonRidePatternSummaryT f = PersonRidePatternSummaryT
  { analysisWindowDays :: (B.C f Kernel.Prelude.Int),
    cancelledTaxiRides :: (B.C f Kernel.Prelude.Int),
    completedTaxiRides :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    dayOfWeekCounts :: (B.C f Data.Aeson.Value),
    freeFormSummary :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    hourOfDayCounts :: (B.C f Data.Aeson.Value),
    lastSummarizedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    lastTripAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    medianFareByService :: (B.C f Data.Aeson.Value),
    medianTipValue :: (B.C f Data.Aeson.Value),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    publicTransportTrips :: (B.C f Kernel.Prelude.Int),
    routePatterns :: (B.C f Data.Aeson.Value),
    scheduledTaxiRides :: (B.C f Kernel.Prelude.Int),
    serviceTakenCounts :: (B.C f Data.Aeson.Value),
    tipComfortability :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    totalTrips :: (B.C f Kernel.Prelude.Int),
    uniqueRoutesTravelled :: (B.C f Kernel.Prelude.Int),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    windowEnd :: (B.C f Kernel.Prelude.UTCTime),
    windowStart :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonRidePatternSummaryT where
  data PrimaryKey PersonRidePatternSummaryT f = PersonRidePatternSummaryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonRidePatternSummaryId . personId

type PersonRidePatternSummary = PersonRidePatternSummaryT Identity

$(enableKVPG (''PersonRidePatternSummaryT) [('personId)] [])

$(mkTableInstances (''PersonRidePatternSummaryT) "person_ride_pattern_summary")
