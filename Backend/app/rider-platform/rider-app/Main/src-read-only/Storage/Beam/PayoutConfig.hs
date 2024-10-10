{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PayoutConfig
import qualified Domain.Types.UtilsTH
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
    payoutEntity :: B.C f Domain.Types.PayoutConfig.PayoutEntity,
    remark :: B.C f Kernel.Prelude.Text,
    timeDiff :: B.C f Kernel.Types.Common.Seconds,
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

$(Domain.Types.UtilsTH.mkCacParseInstance ''PayoutConfigT)
