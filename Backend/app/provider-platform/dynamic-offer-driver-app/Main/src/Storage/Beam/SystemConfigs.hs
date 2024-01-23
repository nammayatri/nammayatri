{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.SystemConfigs where

import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH
import Kernel.Storage.Beam.SystemConfigs as Reexport

instance HasSchemaName SystemConfigsT where
  schemaName _ = T.pack "atlas_driver_offer_bpp"
