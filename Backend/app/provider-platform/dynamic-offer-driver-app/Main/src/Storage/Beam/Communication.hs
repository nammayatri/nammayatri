{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Communication (module Lib.Communication.Storage.Beam.Communication) where

import qualified Data.Text as T
import Lib.Communication.Storage.Beam.Communication
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName CommunicationT where
  schemaName _ = T.pack currentSchemaName
