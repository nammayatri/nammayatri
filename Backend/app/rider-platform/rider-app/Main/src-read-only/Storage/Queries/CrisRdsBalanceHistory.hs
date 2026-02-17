{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CrisRdsBalanceHistory where

import qualified Domain.Types.CrisRdsBalanceHistory
import qualified Domain.Types.IntegratedBPPConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CrisRdsBalanceHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory -> m ())
create = createWithKV

findAllByIntegratedBppConfigId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory])
findAllByIntegratedBppConfigId integratedBppConfigId = do findAllWithKV [Se.And [Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]

findByDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory])
findByDate dateIst = do findAllWithKV [Se.And [Se.Is Beam.dateIst $ Se.Eq dateIst]]

findByDateAndConfig ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m (Maybe Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory))
findByDateAndConfig dateIst integratedBppConfigId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.dateIst $ Se.Eq dateIst,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory -> m (Maybe Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CrisRdsBalanceHistory Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory where
  fromTType' (Beam.CrisRdsBalanceHistoryT {..}) = do
    pure $
      Just
        Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory
          { balance = balance,
            createdAt = createdAt,
            dateIst = dateIst,
            executionTime = executionTime,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CrisRdsBalanceHistory Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory where
  toTType' (Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory {..}) = do
    Beam.CrisRdsBalanceHistoryT
      { Beam.balance = balance,
        Beam.createdAt = createdAt,
        Beam.dateIst = dateIst,
        Beam.executionTime = executionTime,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.updatedAt = updatedAt
      }
