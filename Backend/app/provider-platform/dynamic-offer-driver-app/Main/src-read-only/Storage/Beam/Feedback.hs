{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Feedback where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FeedbackT f = FeedbackT
  { badge :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    rideId :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table FeedbackT where
  data PrimaryKey FeedbackT f = FeedbackId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FeedbackId . id

type Feedback = FeedbackT Identity

$(enableKVPG ''FeedbackT ['id] [])

$(mkTableInstances ''FeedbackT "feedback")

{-
	DSL Source Link: file://./../../../spec/Storage/FeedBack.yaml
-}
