{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.UiRiderConfig where

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

data UiRiderConfigT f = UiRiderConfigT
  { config :: (B.C f Data.Aeson.Value),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Data.Text.Text),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    os :: (B.C f Kernel.Types.Version.DeviceType),
    platform :: (B.C f Lib.Yudhishthira.Types.PlatformType),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table UiRiderConfigT where
  data PrimaryKey UiRiderConfigT f = UiRiderConfigId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = UiRiderConfigId . id

type UiRiderConfig = UiRiderConfigT Identity

$(enableKVPG (''UiRiderConfigT) [('id)] [])

$(mkTableInstances (''UiRiderConfigT) "ui_rider_config")
