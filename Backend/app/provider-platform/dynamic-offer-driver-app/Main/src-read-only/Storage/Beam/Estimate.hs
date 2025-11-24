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
  { businessDiscount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    congestionMultiplier :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    dpVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverExtraFeeDefaultStepFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverExtraFeeDistanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverExtraFeeMaxFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverExtraFeeMinFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverExtraFeeStartDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    driverExtraFeeStepFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    eligibleForUpgrade :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    estimatedDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    estimatedDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    fareParamsId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    farePolicyId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fromLocGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isBlockedRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isCustomerPrefferedSearchRoute :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isScheduled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    maxFare :: B.C f Kernel.Types.Common.Money,
    maxFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    mbActualQARCity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbActualQARCityPast :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbActualQARFromLocGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbActualQARFromLocGeohashDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbActualQARFromLocGeohashDistancePast :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbActualQARFromLocGeohashPast :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbCongestionCity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbCongestionCityPast :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbCongestionFromLocGeohash :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbCongestionFromLocGeohashDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbCongestionFromLocGeohashDistancePast :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mbCongestionFromLocGeohashPast :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
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
