{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantConfig where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.SlidingWindowCounters
import Tools.Beam.UtilsTH

data MerchantConfigT f = MerchantConfigT
  { createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    enabled :: B.C f Kernel.Prelude.Bool,
    fraudBookingCancellationCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudBookingCancellationCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudBookingCancelledByDriverCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudBookingCancelledByDriverCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudBookingTotalCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudRideCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudRideCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudSearchCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudSearchCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigT where
  data PrimaryKey MerchantConfigT f = MerchantConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantConfigId . id

type MerchantConfig = MerchantConfigT Identity

$(enableKVPG ''MerchantConfigT ['id] [['merchantOperatingCityId]])

$(mkTableInstances ''MerchantConfigT "merchant_config")

{-
	DSL Source Link: file://./../../../spec/Storage/MerchantConfig.yaml
-}
