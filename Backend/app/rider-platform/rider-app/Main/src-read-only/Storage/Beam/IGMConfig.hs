{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IGMConfig where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IGMConfigT f = IGMConfigT
  { expectedResolutionTime :: B.C f Kernel.Prelude.Int,
    expectedResponseTime :: B.C f Kernel.Prelude.Int,
    groEmail :: B.C f Data.Text.Text,
    groName :: B.C f Data.Text.Text,
    groPhone :: B.C f Data.Text.Text,
    id :: B.C f Data.Text.Text,
    merchantId :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IGMConfigT where
  data PrimaryKey IGMConfigT f = IGMConfigId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = IGMConfigId . id

type IGMConfig = IGMConfigT Identity

$(enableKVPG ''IGMConfigT ['id] [['merchantId]])

$(mkTableInstances ''IGMConfigT "igm_config")
