{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SystemConfigs where

import qualified Data.Text as T
import Kernel.Storage.Beam.SystemConfigs as Reexport
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName SystemConfigsT where
  schemaName _ = T.pack currentSchemaName
