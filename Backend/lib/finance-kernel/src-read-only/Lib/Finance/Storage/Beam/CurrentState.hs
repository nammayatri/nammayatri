{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Finance.Storage.Beam.CurrentState where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Lib.Finance.Domain.Types.StateTransition
import qualified Kernel.Prelude
import qualified Database.Beam as B



data CurrentStateT f
    = CurrentStateT {currentState :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentState),
                     entityId :: (B.C f Kernel.Prelude.Text),
                     entityType :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentEntityType),
                     merchantId :: (B.C f Kernel.Prelude.Text),
                     merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                     updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                     createdAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table CurrentStateT
    where data PrimaryKey CurrentStateT f = CurrentStateId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CurrentStateId . entityId
type CurrentState = CurrentStateT Identity

$(enableKVPG (''CurrentStateT) [('entityId)] [])

$(mkTableInstancesGenericSchema (''CurrentStateT) "finance_current_state")

