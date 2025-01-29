{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicElement where

import qualified Data.Aeson
import qualified Data.String.Conversions
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement

instance FromTType' Beam.AppDynamicLogicElement Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement where
  fromTType' (Beam.AppDynamicLogicElementT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement
          { description = description,
            domain = domain,
            logic = (Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs) logic,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            order = order,
            version = version,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogicElement Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement where
  toTType' (Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement {..}) = do
    Beam.AppDynamicLogicElementT
      { Beam.description = description,
        Beam.domain = domain,
        Beam.logic = (Data.String.Conversions.cs . Data.Aeson.encode) logic,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.order = order,
        Beam.version = version,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
