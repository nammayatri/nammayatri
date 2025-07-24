{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedEstimate where

import qualified Data.Time.LocalTime
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedEstimate
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data SharedEstimateT f = SharedEstimateT
  { bppSharedEstimateId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    estimateIds :: (B.C f [Kernel.Prelude.Text]),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters)),
    estimatedDistanceValue :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance)),
    estimatedDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    estimatedTotalFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    nightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    nightShiftChargeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    nightShiftEnd :: (B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay)),
    nightShiftStart :: (B.C f (Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay)),
    oldNightShiftCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal)),
    providerId :: (B.C f Kernel.Prelude.Text),
    providerName :: (B.C f Kernel.Prelude.Text),
    providerUrl :: (B.C f Kernel.Prelude.Text),
    serviceTierName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sharedSearchRequestId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SharedEstimate.EstimateStatus),
    totalFareRangeMax :: (B.C f Kernel.Types.Common.HighPrecMoney),
    totalFareRangeMin :: (B.C f Kernel.Types.Common.HighPrecMoney),
    tripCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleServiceTierSeatingCapacity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    vehicleVariant :: (B.C f Domain.Types.ServiceTierType.ServiceTierType)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedEstimateT where
  data PrimaryKey SharedEstimateT f = SharedEstimateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedEstimateId . id

type SharedEstimate = SharedEstimateT Identity

$(enableKVPG (''SharedEstimateT) [('id)] [[('sharedSearchRequestId)]])

$(mkTableInstances (''SharedEstimateT) "shared_estimate")
