{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Beam.SystemConfigs where

import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH
import Kernel.Storage.Beam.SystemConfigs as Reexport

instance HasSchemaName SystemConfigsT where
  schemaName _ = T.pack "yudhishthira"
