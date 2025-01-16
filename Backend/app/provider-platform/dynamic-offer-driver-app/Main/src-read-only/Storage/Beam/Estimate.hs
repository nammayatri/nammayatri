{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Estimate where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data EstimateT f = EstimateT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    dpVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    eligibleForUpgrade :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    fareParamsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    farePolicyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isBlockedRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isCustomerPrefferedSearchRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxFare :: B.C f Kernel.Types.Common.Money,
    maxFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    minFare :: B.C f Kernel.Types.Common.Money,
    minFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    requestId :: B.C f Kernel.Prelude.Text,
    smartTipReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    smartTipSuggestion :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    specialLocationTag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    supplyDemandRatioFromLoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    supplyDemandRatioToLoc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    tipOptions :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int]),
    tollNames :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    tripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    vehicleVariant :: B.C f Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateT where
  data PrimaryKey EstimateT f = EstimateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EstimateId . id

type Estimate = EstimateT Identity

$(enableKVPG ''EstimateT ['id] [['requestId]])

$(mkTableInstancesWithTModifier ''EstimateT "estimate" [])
