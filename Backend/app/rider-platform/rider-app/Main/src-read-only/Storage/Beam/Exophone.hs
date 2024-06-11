{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Exophone where

import qualified Database.Beam as B
import qualified Kernel.External.Call.Types
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ExophoneT f = ExophoneT
  { backupPhone :: B.C f Kernel.Prelude.Text,
    callService :: B.C f Kernel.External.Call.Types.CallService,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    isPrimaryDown :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    primaryPhone :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ExophoneT where
  data PrimaryKey ExophoneT f = ExophoneId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ExophoneId . id

type Exophone = ExophoneT Identity

$(enableKVPG ''ExophoneT ['id] [['backupPhone], ['merchantOperatingCityId], ['primaryPhone]])

$(mkTableInstances ''ExophoneT "exophone")

{-
	DSL Source Link: file://./../../../spec/Storage/exophone.yaml
-}
