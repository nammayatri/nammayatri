{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.CrisRdsBalanceHistory where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.CrisRdsBalanceHistory
import qualified Storage.Beam.CrisRdsBalanceHistory as Beam
import qualified Kernel.Types.Common
import qualified Kernel.Prelude
import qualified Data.Time.Calendar
import qualified Kernel.Types.Id
import qualified Domain.Types.IntegratedBPPConfig
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory -> m ())
create = createWithKV
findAllByIntegratedBppConfigId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ([Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory]))
findAllByIntegratedBppConfigId integratedBppConfigId = do findAllWithKV [Se.And [Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]
findByDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Data.Time.Calendar.Day -> m ([Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory]))
findByDate dateIst = do findAllWithKV [Se.And [Se.Is Beam.dateIst $ Se.Eq dateIst]]
findByDateAndConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                       (Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ([Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory]))
findByDateAndConfig dateIst integratedBppConfigId = do findAllWithKV [Se.And [Se.Is Beam.dateIst $ Se.Eq dateIst,
                                                                              Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]
updateBalanceAndExecutionTimeByDateAndConfig :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                (Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
updateBalanceAndExecutionTimeByDateAndConfig balance executionTime dateIst integratedBppConfigId = do {_now <- getCurrentTime;
                                                                                                       updateWithKV [Se.Set Beam.balance balance, Se.Set Beam.executionTime executionTime, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.dateIst $ Se.Eq dateIst,
                                                                                                                                                                                                                                Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory -> m (Maybe Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



instance FromTType' Beam.CrisRdsBalanceHistory Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory
    where fromTType' (Beam.CrisRdsBalanceHistoryT {..}) = do pure $ Just Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory{balance = balance,
                                                                                                                                  createdAt = createdAt,
                                                                                                                                  dateIst = dateIst,
                                                                                                                                  executionTime = executionTime,
                                                                                                                                  id = Kernel.Types.Id.Id id,
                                                                                                                                  integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
                                                                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.CrisRdsBalanceHistory Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory
    where toTType' (Domain.Types.CrisRdsBalanceHistory.CrisRdsBalanceHistory {..}) = do Beam.CrisRdsBalanceHistoryT{Beam.balance = balance,
                                                                                                                    Beam.createdAt = createdAt,
                                                                                                                    Beam.dateIst = dateIst,
                                                                                                                    Beam.executionTime = executionTime,
                                                                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                                                                    Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
                                                                                                                    Beam.updatedAt = updatedAt}



