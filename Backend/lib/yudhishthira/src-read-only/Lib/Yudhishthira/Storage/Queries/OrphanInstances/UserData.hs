{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.OrphanInstances.UserData where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.UserData as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.UserData

instance FromTType' Beam.UserData Lib.Yudhishthira.Types.UserData.UserData where
  fromTType' (Beam.UserDataT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.UserData.UserData
          { batchNumber = batchNumber,
            chakra = chakra,
            eventId = Kernel.Types.Id.Id eventId,
            id = Kernel.Types.Id.Id id,
            userDataValue = userDataValue,
            userId = Kernel.Types.Id.Id userId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.UserData Lib.Yudhishthira.Types.UserData.UserData where
  toTType' (Lib.Yudhishthira.Types.UserData.UserData {..}) = do
    Beam.UserDataT
      { Beam.batchNumber = batchNumber,
        Beam.chakra = chakra,
        Beam.eventId = Kernel.Types.Id.getId eventId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.userDataValue = userDataValue,
        Beam.userId = Kernel.Types.Id.getId userId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
