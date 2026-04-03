{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.CallStatus where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.CallStatus
import qualified Kernel.External.Call.Types
import qualified Kernel.External.Call.Interface.Types
import qualified Database.Beam as B



data CallStatusT f
    = CallStatusT {callAttempt :: (B.C f (Kernel.Prelude.Maybe Domain.Types.CallStatus.CallAttemptStatus)),
                   callError :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   callId :: (B.C f Kernel.Prelude.Text),
                   callService :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService)),
                   conversationDuration :: (B.C f Kernel.Prelude.Int),
                   createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   customerIvrResponse :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   dtmfNumberUsed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   id :: (B.C f Kernel.Prelude.Text),
                   merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                   recordingUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                   rideId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                   status :: (B.C f Kernel.External.Call.Interface.Types.CallStatus),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table CallStatusT
    where data PrimaryKey CallStatusT f = CallStatusId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = CallStatusId . id
type CallStatus = CallStatusT Identity

$(enableKVPG (''CallStatusT) [('id)] [[('callId)]])

$(mkTableInstances (''CallStatusT) "call_status")

