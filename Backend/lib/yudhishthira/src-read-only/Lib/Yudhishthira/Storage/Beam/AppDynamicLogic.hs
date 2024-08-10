{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.AppDynamicLogic where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data AppDynamicLogicT f = AppDynamicLogicT
  { description :: B.C f Data.Text.Text,
    domain :: B.C f Data.Text.Text,
    logic :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    name :: B.C f Data.Text.Text,
    order :: B.C f Kernel.Prelude.Int,
    timeBounds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AppDynamicLogicT where
  data PrimaryKey AppDynamicLogicT f = AppDynamicLogicId (B.C f Data.Text.Text) (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = AppDynamicLogicId <$> domain <*> merchantOperatingCityId <*> name

type AppDynamicLogic = AppDynamicLogicT Identity

$(enableKVPG ''AppDynamicLogicT ['domain, 'merchantOperatingCityId, 'name] [])

$(mkTableInstancesGenericSchema ''AppDynamicLogicT "app_dynamic_logic")
