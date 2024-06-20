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
  { id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    fraudBookingCancellationCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudBookingCancellationCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudBookingTotalCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudBookingCancelledByDriverCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudBookingCancelledByDriverCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudSearchCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudSearchCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    fraudRideCountThreshold :: B.C f Kernel.Prelude.Int,
    fraudRideCountWindow :: B.C f Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    updatedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    enabled :: B.C f Kernel.Prelude.Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigT where
  data PrimaryKey MerchantConfigT f = MerchantConfigId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantConfigId . id

type MerchantConfig = MerchantConfigT Identity

$(enableKVPG ''MerchantConfigT ['id] [['merchantOperatingCityId]])

$(mkTableInstances ''MerchantConfigT "merchant_config")
