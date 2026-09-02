{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareAlertSubscription where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FareAlertSubscription
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FareAlertSubscriptionT f = FareAlertSubscriptionT
  { alertType :: (B.C f Domain.Types.FareAlertSubscription.FareAlertType),
    email :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    subscribedBy :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareAlertSubscriptionT where
  data PrimaryKey FareAlertSubscriptionT f = FareAlertSubscriptionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareAlertSubscriptionId . id

type FareAlertSubscription = FareAlertSubscriptionT Identity

$(enableKVPG (''FareAlertSubscriptionT) [('id)] [])

$(mkTableInstances (''FareAlertSubscriptionT) "fare_alert_subscription")
