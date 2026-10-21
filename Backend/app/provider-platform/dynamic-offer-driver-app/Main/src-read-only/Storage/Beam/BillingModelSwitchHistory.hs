{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BillingModelSwitchHistory where

import qualified Database.Beam as B
import qualified Domain.Types.BillingModelSwitchHistory
import Domain.Types.Common ()
import qualified Domain.Types.Extra.Plan
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data BillingModelSwitchHistoryT f = BillingModelSwitchHistoryT
  { acknowledgedUnusedCredits :: (B.C f Kernel.Prelude.Bool),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    fromModel :: (B.C f Domain.Types.Extra.Plan.ServiceNames),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    requestedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    source :: (B.C f Domain.Types.BillingModelSwitchHistory.BillingModelSwitchSource),
    toModel :: (B.C f Domain.Types.Extra.Plan.ServiceNames),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table BillingModelSwitchHistoryT where
  data PrimaryKey BillingModelSwitchHistoryT f = BillingModelSwitchHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = BillingModelSwitchHistoryId . id

type BillingModelSwitchHistory = BillingModelSwitchHistoryT Identity

$(enableKVPG (''BillingModelSwitchHistoryT) [('id)] [[('driverId)]])

$(mkTableInstances (''BillingModelSwitchHistoryT) "billing_model_switch_history")
