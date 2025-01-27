{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SuspectStatusChangeRequest where

import qualified Database.Beam as B
import qualified Domain.Types.SuspectFlagRequest
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SuspectStatusChangeRequestT f = SuspectStatusChangeRequestT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantShortId :: B.C f Kernel.Prelude.Text,
    reasonToChange :: B.C f Kernel.Prelude.Text,
    reqStatus :: B.C f Domain.Types.SuspectFlagRequest.AdminApproval,
    suspectId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table SuspectStatusChangeRequestT where
  data PrimaryKey SuspectStatusChangeRequestT f = SuspectStatusChangeRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SuspectStatusChangeRequestId . id

type SuspectStatusChangeRequest = SuspectStatusChangeRequestT Identity

$(enableKVPG ''SuspectStatusChangeRequestT ['id] [])

$(mkTableInstances ''SuspectStatusChangeRequestT "suspect_status_change_request")
