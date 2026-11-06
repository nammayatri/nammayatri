{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AppDynamicLogicWindowConfig where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data AppDynamicLogicWindowConfigT f = AppDynamicLogicWindowConfigT
  { domain :: (B.C f Lib.Yudhishthira.Types.LogicDomain),
    version :: (B.C f Kernel.Prelude.Int),
    windowMode :: (B.C f Lib.Yudhishthira.Types.RulebookWindowMode),
    windowSize :: (B.C f Kernel.Prelude.Int),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppDynamicLogicWindowConfigT where
  data PrimaryKey AppDynamicLogicWindowConfigT f = AppDynamicLogicWindowConfigId (B.C f Lib.Yudhishthira.Types.LogicDomain) (B.C f Kernel.Prelude.Int) deriving (Generic, B.Beamable)
  primaryKey = AppDynamicLogicWindowConfigId <$> domain <*> version

type AppDynamicLogicWindowConfig = AppDynamicLogicWindowConfigT Identity

$(enableKVPG (''AppDynamicLogicWindowConfigT) [('domain), ('version)] [[('domain)]])

$(mkTableInstances (''AppDynamicLogicWindowConfigT) "app_dynamic_logic_window_config")
