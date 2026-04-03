{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Finance.Storage.Beam.StateTransition where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Lib.Finance.Domain.Types.StateTransition
import qualified Data.Aeson
import qualified Database.Beam as B



data StateTransitionT f
    = StateTransitionT {actorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        actorType :: (B.C f Kernel.Prelude.Text),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        entityId :: (B.C f Kernel.Prelude.Text),
                        entityType :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentEntityType),
                        event :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentEvent),
                        eventData :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                        fromState :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentState),
                        id :: (B.C f Kernel.Prelude.Text),
                        merchantId :: (B.C f Kernel.Prelude.Text),
                        merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                        toState :: (B.C f Lib.Finance.Domain.Types.StateTransition.PaymentState),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table StateTransitionT
    where data PrimaryKey StateTransitionT f = StateTransitionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = StateTransitionId . id
type StateTransition = StateTransitionT Identity

$(enableKVPG (''StateTransitionT) [('id)] [[('entityId)]])

$(mkTableInstancesGenericSchema (''StateTransitionT) "finance_state_transition")

