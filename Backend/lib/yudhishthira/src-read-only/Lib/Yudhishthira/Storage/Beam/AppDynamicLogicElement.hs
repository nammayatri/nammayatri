{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data AppDynamicLogicElementT f = AppDynamicLogicElementT
  { description :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    domain :: B.C f Lib.Yudhishthira.Types.LogicDomain,
    logic :: B.C f Data.Text.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    order :: B.C f Kernel.Prelude.Int,
    patchedElement :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    version :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AppDynamicLogicElementT where
  data PrimaryKey AppDynamicLogicElementT f
    = AppDynamicLogicElementId (B.C f Lib.Yudhishthira.Types.LogicDomain) (B.C f Kernel.Prelude.Int) (B.C f Kernel.Prelude.Int)
    deriving (Generic, B.Beamable)
  primaryKey = AppDynamicLogicElementId <$> domain <*> order <*> version

type AppDynamicLogicElement = AppDynamicLogicElementT Identity

$(enableKVPG ''AppDynamicLogicElementT ['domain, 'order, 'version] [])

$(mkTableInstancesGenericSchema ''AppDynamicLogicElementT "app_dynamic_logic_element")
