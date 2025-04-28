{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CallFeedback where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CallFeedbackT f = CallFeedbackT
  { callId :: (B.C f Kernel.Prelude.Text),
    entityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    optionIds :: (B.C f [Kernel.Prelude.Text]),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table CallFeedbackT where
  data PrimaryKey CallFeedbackT f = CallFeedbackId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CallFeedbackId <$> callId <*> id

type CallFeedback = CallFeedbackT Identity

$(enableKVPG (''CallFeedbackT) [('callId), ('id)] [])

$(mkTableInstances (''CallFeedbackT) "call_feedback")
