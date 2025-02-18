{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.UiDriverConfig where

import qualified Data.Aeson
import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data UiDriverConfigT f = UiDriverConfigT
  { bundleVersion :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
    config :: (B.C f Data.Aeson.Value),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Data.Text.Text),
    language :: (B.C f Kernel.External.Types.Language),
    os :: (B.C f Data.Text.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table UiDriverConfigT where
  data PrimaryKey UiDriverConfigT f = UiDriverConfigId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = UiDriverConfigId . id

type UiDriverConfig = UiDriverConfigT Identity

$(enableKVPG (''UiDriverConfigT) [('id)] [])

$(mkTableInstances (''UiDriverConfigT) "ui_driver_config")
