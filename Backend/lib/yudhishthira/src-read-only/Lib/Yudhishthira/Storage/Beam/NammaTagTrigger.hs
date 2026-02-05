{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.NammaTagTrigger where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data NammaTagTriggerT f = NammaTagTriggerT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    event :: B.C f Lib.Yudhishthira.Types.ApplicationEvent,
    tagName :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table NammaTagTriggerT where
  data PrimaryKey NammaTagTriggerT f = NammaTagTriggerId (B.C f Lib.Yudhishthira.Types.ApplicationEvent) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = NammaTagTriggerId <$> event <*> tagName

type NammaTagTrigger = NammaTagTriggerT Identity

$(enableKVPG ''NammaTagTriggerT ['event, 'tagName] [])

$(mkTableInstancesGenericSchema ''NammaTagTriggerT "namma_tag_trigger")
