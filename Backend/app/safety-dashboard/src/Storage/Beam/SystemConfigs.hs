{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SystemConfigs where

import qualified Data.Text as T
import Kernel.Storage.Beam.SystemConfigs
import Tools.Beam.UtilsTH (HasSchemaName (..))

instance HasSchemaName SystemConfigsT where
  schemaName _ = T.pack "atlas_safety_dashboard"
