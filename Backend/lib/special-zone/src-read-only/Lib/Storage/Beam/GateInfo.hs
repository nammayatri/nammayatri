{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Storage.Beam.GateInfo where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Types.GateInfo

data GateInfoT f = GateInfoT
  { address :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    canQueueUpOnGate :: (B.C f Kernel.Prelude.Bool),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    defaultDemandThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    defaultDriverExtra :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    defaultMaxDriverThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    defaultMinDriverThreshold :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    demandThresholdsJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    enableQueueFilter :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    entryFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    gateTags :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text])),
    gateType :: (B.C f Lib.Types.GateInfo.GateType),
    geomGeoJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    maxDriverThresholdsJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    maxRideSkipsBeforeQueueRemoval :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    minDriverThresholdsJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    name :: (B.C f Kernel.Prelude.Text),
    navigationInstructionsJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    notificationActiveTillInSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    notificationCooldownInSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    pickupRequestResponseTimeoutInSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    pickupZoneArrivalTimeoutInSec :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    point :: (B.C f Kernel.Prelude.Text),
    specialLocationId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    walkDescription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table GateInfoT where
  data PrimaryKey GateInfoT f = GateInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = GateInfoId . id

type GateInfo = GateInfoT Identity

$(enableKVPG (''GateInfoT) [('id)] [[('specialLocationId)]])

$(mkTableInstancesGenericSchema (''GateInfoT) "gate_info")
