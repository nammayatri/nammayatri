{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PayoutConfig
import qualified Domain.Types.UtilsTH
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Payout.Interface.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PayoutConfigT f = PayoutConfigT
  { batchLimit :: B.C f Kernel.Prelude.Int,
    expand :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payout.Interface.Types.Expand),
    id :: B.C f Kernel.Prelude.Text,
    isPayoutEnabled :: B.C f Kernel.Prelude.Bool,
    maxPayoutReferralForADay :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    maxRetryCount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    orderType :: B.C f Kernel.Prelude.Text,
    payoutEntity :: B.C f Domain.Types.PayoutConfig.PayoutEntity,
    referralRewardAmountPerRide :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    referredByRewardAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    remark :: B.C f Kernel.Prelude.Text,
    thresholdPayoutAmountPerPerson :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    timeDiff :: B.C f Kernel.Types.Common.Seconds,
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutConfigT where
  data PrimaryKey PayoutConfigT f = PayoutConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PayoutConfigId . id

type PayoutConfig = PayoutConfigT Identity

$(enableKVPG ''PayoutConfigT ['id] [])

$(mkTableInstances ''PayoutConfigT "payout_config")

$(Domain.Types.UtilsTH.mkCacParseInstance ''PayoutConfigT)
