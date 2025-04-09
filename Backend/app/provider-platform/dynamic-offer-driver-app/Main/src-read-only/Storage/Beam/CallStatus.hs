{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CallStatus where

import qualified Database.Beam as B
import qualified Domain.Types.CallStatus
import Domain.Types.Common ()
import qualified Kernel.External.Call.Interface.Types
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CallStatusT f = CallStatusT
  { callAttempt :: (B.C f (Kernel.Prelude.Maybe Domain.Types.CallStatus.CallAttemptStatus)),
    callError :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    callId :: (B.C f Kernel.Prelude.Text),
    callService :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Call.Types.CallService)),
    conversationDuration :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    dtmfNumberUsed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    entityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    recordingUrl :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Kernel.External.Call.Interface.Types.CallStatus)
  }
  deriving (Generic, B.Beamable)

instance B.Table CallStatusT where
  data PrimaryKey CallStatusT f = CallStatusId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CallStatusId . id

type CallStatus = CallStatusT Identity

$(enableKVPG (''CallStatusT) [('id)] [[('callId)]])

$(mkTableInstances (''CallStatusT) "call_status")
