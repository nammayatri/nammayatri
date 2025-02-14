{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.UiDriverConfig where

import qualified Data.Aeson
import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Version
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data UiDriverConfigT f = UiDriverConfigT
  { config :: (B.C f Data.Aeson.Value),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Data.Text.Text),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    os :: (B.C f Kernel.Types.Version.DeviceType),
    platform :: (B.C f Lib.Yudhishthira.Types.PlatformType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table UiDriverConfigT where
  data PrimaryKey UiDriverConfigT f = UiDriverConfigId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = UiDriverConfigId . id

type UiDriverConfig = UiDriverConfigT Identity

$(enableKVPG (''UiDriverConfigT) [('id)] [])

$(mkTableInstances (''UiDriverConfigT) "ui_driver_config")
