{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutConfig where

import qualified Database.Beam as B
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PayoutConfigT f = PayoutConfigT
  { batchLimit :: B.C f Kernel.Prelude.Int,
    isPayoutEnabled :: B.C f Kernel.Prelude.Bool,
    maxRetryCount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    orderType :: B.C f Kernel.Prelude.Text,
    payoutRegistrationCgst :: B.C f Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationFee :: B.C f Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationSgst :: B.C f Kernel.Types.Common.HighPrecMoney,
    referralRewardAmountPerRide :: B.C f Kernel.Types.Common.HighPrecMoney,
    remark :: B.C f Kernel.Prelude.Text,
    thresholdPayoutAmountPerPerson :: B.C f Kernel.Types.Common.HighPrecMoney,
    timeDiff :: B.C f Kernel.Types.Common.Seconds,
    vehicleVariant :: B.C f Domain.Types.Vehicle.Variant,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutConfigT where
  data PrimaryKey PayoutConfigT f = PayoutConfigId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.Vehicle.Variant) deriving (Generic, B.Beamable)
  primaryKey = PayoutConfigId <$> merchantOperatingCityId <*> vehicleVariant

type PayoutConfig = PayoutConfigT Identity

$(enableKVPG ''PayoutConfigT ['merchantOperatingCityId, 'vehicleVariant] [])

$(mkTableInstances ''PayoutConfigT "payout_config")
