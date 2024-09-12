{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.UserData where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.UserData as Beam
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.UserData
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.UserData.UserData -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.UserData.UserData] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Yudhishthira.Types.UserData.UserData -> m (Maybe Lib.Yudhishthira.Types.UserData.UserData))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.UserData.UserData -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.UserData.UserData {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.chakra chakra,
      Se.Set Beam.userDataValue userDataValue,
      Se.Set Beam.userId (Kernel.Types.Id.getId userId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.UserData Lib.Yudhishthira.Types.UserData.UserData where
  fromTType' (Beam.UserDataT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.UserData.UserData
          { chakra = chakra,
            id = Kernel.Types.Id.Id id,
            userDataValue = userDataValue,
            userId = Kernel.Types.Id.Id userId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.UserData Lib.Yudhishthira.Types.UserData.UserData where
  toTType' (Lib.Yudhishthira.Types.UserData.UserData {..}) = do
    Beam.UserDataT
      { Beam.chakra = chakra,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.userDataValue = userDataValue,
        Beam.userId = Kernel.Types.Id.getId userId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
