{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VoipCallStatus where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH
import qualified Utils.Common.Voip.Types.VoipStorageType

data VoipCallStatusT f = VoipCallStatusT
  { callId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    callStatus :: B.C f Utils.Common.Voip.Types.VoipStorageType.VoipStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    errorCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    id :: B.C f Kernel.Prelude.Text,
    merchantCity :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    networkQuality :: B.C f Kernel.Prelude.Text,
    networkType :: B.C f Kernel.Prelude.Text,
    rideId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VoipCallStatusT where
  data PrimaryKey VoipCallStatusT f = VoipCallStatusId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = VoipCallStatusId . id

type VoipCallStatus = VoipCallStatusT Identity

$(enableKVPG ''VoipCallStatusT ['id] [['rideId]])

$(mkTableInstances ''VoipCallStatusT "voip_call_status")
