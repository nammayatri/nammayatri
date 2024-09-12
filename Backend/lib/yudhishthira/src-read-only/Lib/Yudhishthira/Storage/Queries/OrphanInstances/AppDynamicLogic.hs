{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogic where

import qualified Data.Aeson
import qualified Data.String.Conversions
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogic as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogic

instance FromTType' Beam.AppDynamicLogic Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic where
  fromTType' (Beam.AppDynamicLogicT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic
          { description = description,
            domain = domain,
            logic = ((Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs)) logic,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            timeBounds = (Kernel.Prelude.fromMaybe Kernel.Types.TimeBound.Unbounded) timeBounds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogic Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic where
  toTType' (Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic {..}) = do
    Beam.AppDynamicLogicT
      { Beam.description = description,
        Beam.domain = domain,
        Beam.logic = ((Data.String.Conversions.cs . Data.Aeson.encode)) logic,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.timeBounds = Kernel.Prelude.Just timeBounds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
