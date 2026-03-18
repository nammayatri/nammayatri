{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.AppDynamicLogicAlwaysOn where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data AppDynamicLogicAlwaysOnT f = AppDynamicLogicAlwaysOnT
  { domain :: (B.C f Lib.Yudhishthira.Types.LogicDomain),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    order :: (B.C f Kernel.Prelude.Int),
    version :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppDynamicLogicAlwaysOnT where
  data PrimaryKey AppDynamicLogicAlwaysOnT f = AppDynamicLogicAlwaysOnId (B.C f Lib.Yudhishthira.Types.LogicDomain) (B.C f Data.Text.Text) (B.C f Kernel.Prelude.Int) deriving (Generic, B.Beamable)
  primaryKey = AppDynamicLogicAlwaysOnId <$> domain <*> merchantOperatingCityId <*> version

type AppDynamicLogicAlwaysOn = AppDynamicLogicAlwaysOnT Identity

$(enableKVPG (''AppDynamicLogicAlwaysOnT) [('domain), ('merchantOperatingCityId), ('version)] [])

$(mkTableInstancesGenericSchema (''AppDynamicLogicAlwaysOnT) "app_dynamic_logic_always_on")
