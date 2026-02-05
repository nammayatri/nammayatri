{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.CurrentState where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Finance.Domain.Types.StateTransition

data CurrentStateT f = CurrentStateT
  { currentState :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentState),
    entityId :: (B.C f Kernel.Prelude.Text),
    entityType :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    createdAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table CurrentStateT where
  data PrimaryKey CurrentStateT f = CurrentStateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CurrentStateId . entityId

type CurrentState = CurrentStateT Identity

$(enableKVPG (''CurrentStateT) [('entityId)] [])

$(mkTableInstancesGenericSchema (''CurrentStateT) "finance_current_state")
