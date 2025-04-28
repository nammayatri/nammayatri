{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CallFeedbackOptions where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CallFeedbackOptionsT f = CallFeedbackOptionsT
  { category :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    messageKey :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table CallFeedbackOptionsT where
  data PrimaryKey CallFeedbackOptionsT f = CallFeedbackOptionsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CallFeedbackOptionsId . id

type CallFeedbackOptions = CallFeedbackOptionsT Identity

$(enableKVPG (''CallFeedbackOptionsT) [('id)] [])

$(mkTableInstances (''CallFeedbackOptionsT) "call_feedback_options")
