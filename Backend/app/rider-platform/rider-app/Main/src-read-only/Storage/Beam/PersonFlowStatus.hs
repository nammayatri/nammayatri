{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonFlowStatus where

import qualified Database.Beam as B
import qualified Domain.Types.Extra.PersonFlowStatus
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonFlowStatusT f = PersonFlowStatusT {flowStatus :: B.C f Domain.Types.Extra.PersonFlowStatus.FlowStatus, personId :: B.C f Kernel.Prelude.Text, updatedAt :: B.C f Kernel.Prelude.UTCTime}
  deriving (Generic, B.Beamable)

instance B.Table PersonFlowStatusT where
  data PrimaryKey PersonFlowStatusT f = PersonFlowStatusId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonFlowStatusId . personId

type PersonFlowStatus = PersonFlowStatusT Identity

$(enableKVPG ''PersonFlowStatusT ['personId] [])

$(mkTableInstances ''PersonFlowStatusT "person_flow_status")

{-
	DSL Source Link: file://./../../../spec/Storage/PersonFlowStatus.yaml
-}
