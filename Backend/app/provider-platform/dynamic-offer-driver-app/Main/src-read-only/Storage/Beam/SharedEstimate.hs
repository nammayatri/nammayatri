{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SharedEstimate where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedEstimate
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data SharedEstimateT f = SharedEstimateT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Utils.Common.Currency),
    distanceUnit :: (B.C f Kernel.Types.Common.DistanceUnit),
    estimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    estimatedDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    status :: (B.C f Domain.Types.SharedEstimate.EstimateStatus),
    tollNames :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    totalMaxFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    totalMinFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    transactionId :: (B.C f Kernel.Prelude.Text),
    tripCategory :: (B.C f Domain.Types.Common.TripCategory),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    validTill :: (B.C f Kernel.Prelude.UTCTime),
    vehicleServiceTier :: (B.C f Domain.Types.ServiceTierType.ServiceTierType)
  }
  deriving (Generic, B.Beamable)

instance B.Table SharedEstimateT where
  data PrimaryKey SharedEstimateT f = SharedEstimateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SharedEstimateId . id

type SharedEstimate = SharedEstimateT Identity

$(enableKVPG (''SharedEstimateT) [('id)] [])

$(mkTableInstances (''SharedEstimateT) "shared_estimate")
