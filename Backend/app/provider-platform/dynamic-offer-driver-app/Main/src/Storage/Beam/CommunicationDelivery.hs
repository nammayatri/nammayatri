{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.CommunicationDelivery (module Lib.Communication.Storage.Beam.CommunicationDelivery) where

import qualified Data.Text as T
import Lib.Communication.Storage.Beam.CommunicationDelivery
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName CommunicationDeliveryT where
  schemaName _ = T.pack currentSchemaName
