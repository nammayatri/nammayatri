{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutConfig where

import qualified Database.Beam as B
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PayoutConfigT f = PayoutConfigT
  { batchLimit :: B.C f Kernel.Prelude.Int,
    isPayoutEnabled :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    payoutRegistrationAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    referralRewardAmountPerRide :: B.C f Kernel.Types.Common.HighPrecMoney,
    thresholdPayoutAmountPerPerson :: B.C f Kernel.Types.Common.HighPrecMoney,
    timeDiff :: B.C f Kernel.Types.Common.Seconds,
    vehicleServiceTier :: B.C f (Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType),
    vehicleVariant :: B.C f Domain.Types.Vehicle.Variant,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutConfigT where
  data PrimaryKey PayoutConfigT f = PayoutConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutConfigId . merchantOperatingCityId

type PayoutConfig = PayoutConfigT Identity

$(enableKVPG ''PayoutConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''PayoutConfigT "payout_config")
