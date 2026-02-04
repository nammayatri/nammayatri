{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.CurrentState where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.CurrentState
import qualified Lib.Finance.Domain.Types.StateTransition
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.CurrentState as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.CurrentState.CurrentState -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.CurrentState.CurrentState] -> m ())
createMany = traverse_ create

findByEntity :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.CurrentState.CurrentState))
findByEntity entityType entityId = do findOneWithKV [Se.And [Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.entityId $ Se.Eq entityId]]

updateState :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.StateTransition.PaymentState -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateState currentState entityType entityId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.currentState currentState, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.entityId $ Se.Eq entityId]]

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Finance.Domain.Types.CurrentState.CurrentState))
findByPrimaryKey entityId = do findOneWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.CurrentState.CurrentState -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.CurrentState.CurrentState {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currentState currentState,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.entityId $ Se.Eq entityId]]

instance FromTType' Beam.CurrentState Lib.Finance.Domain.Types.CurrentState.CurrentState where
  fromTType' (Beam.CurrentStateT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.CurrentState.CurrentState
          { currentState = currentState,
            entityId = entityId,
            entityType = entityType,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' Beam.CurrentState Lib.Finance.Domain.Types.CurrentState.CurrentState where
  toTType' (Lib.Finance.Domain.Types.CurrentState.CurrentState {..}) = do
    Beam.CurrentStateT
      { Beam.currentState = currentState,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.updatedAt = updatedAt,
        Beam.createdAt = createdAt
      }
