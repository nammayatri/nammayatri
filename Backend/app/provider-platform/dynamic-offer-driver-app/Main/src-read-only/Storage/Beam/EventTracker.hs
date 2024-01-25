{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EventTracker where

import qualified Database.Beam as B
import qualified Domain.Types.EventTracker
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data EventTrackerT f = EventTrackerT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    entity :: B.C f Kernel.Prelude.Text,
    entityFieldName :: B.C f Kernel.Prelude.Text,
    entityPrimaryId :: B.C f Kernel.Prelude.Text,
    eventName :: B.C f Domain.Types.EventTracker.EventName,
    fromState :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    reason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    subscriptionServiceName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    toState :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EventTrackerT where
  data PrimaryKey EventTrackerT f = EventTrackerId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = EventTrackerId . id

type EventTracker = EventTrackerT Identity

$(enableKVPG ''EventTrackerT ['id] [])

$(mkTableInstances ''EventTrackerT "event_tracker")
