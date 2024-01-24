{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RiderConfig where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data RiderConfigT f = RiderConfigT
  { appUrl :: B.C f Kernel.Prelude.Text,
    enableEmergencyContactAddedMessage :: B.C f Kernel.Prelude.Bool,
    enableLocalPoliceSupport :: B.C f Kernel.Prelude.Bool,
    enableSupportForSafety :: B.C f Kernel.Prelude.Bool,
    localPoliceNumber :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    safetyCheckEndTime :: B.C f Kernel.Types.Common.Seconds,
    safetyCheckStartTime :: B.C f Kernel.Types.Common.Seconds,
    timeDiffFromUtc :: B.C f Kernel.Types.Common.Seconds,
    trackingShortUrlPattern :: B.C f Kernel.Prelude.Text,
    videoFileSizeUpperLimit :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderConfigT where
  data PrimaryKey RiderConfigT f = RiderConfigId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = RiderConfigId . merchantOperatingCityId

type RiderConfig = RiderConfigT Identity

$(enableKVPG ''RiderConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''RiderConfigT "rider_config")
