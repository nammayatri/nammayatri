{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.NyRegularInstanceLog where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.NyRegularInstanceLog
import qualified Storage.Beam.NyRegularInstanceLog as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.NyRegularSubscription
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog] -> m ())
createMany = traverse_ create
findByInstanceTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog))
findByInstanceTransactionId instanceTransactionId = do findOneWithKV [Se.Is Beam.instanceTransactionId $ Se.Eq instanceTransactionId]
findBySubscriptionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                        (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> m ([Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog]))
findBySubscriptionId limit offset nyRegularSubscriptionId = do findAllWithOptionsKV [Se.Is Beam.nyRegularSubscriptionId $ Se.Eq (Kernel.Types.Id.getId nyRegularSubscriptionId)] (Se.Desc Beam.scheduledPickupTime) limit offset
findBySubscriptionIdAndScheduledTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                        (Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription -> Kernel.Prelude.UTCTime -> m (Maybe Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog))
findBySubscriptionIdAndScheduledTime nyRegularSubscriptionId scheduledPickupTime = do findOneWithKV [Se.And [Se.Is Beam.nyRegularSubscriptionId $ Se.Eq (Kernel.Types.Id.getId nyRegularSubscriptionId),
                                                                                                             Se.Is Beam.scheduledPickupTime $ Se.Eq scheduledPickupTime]]
updateStatusByInstanceTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularInstanceLog.NyRegularInstanceAutomationStatus -> Kernel.Prelude.Text -> m ())
updateStatusByInstanceTransactionId automationStatus instanceTransactionId = do {_now <- getCurrentTime;
                                                                                 updateWithKV [Se.Set Beam.automationStatus automationStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.instanceTransactionId $ Se.Eq instanceTransactionId]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog))
findByPrimaryKey instanceTransactionId = do findOneWithKV [Se.And [Se.Is Beam.instanceTransactionId $ Se.Eq instanceTransactionId]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog -> m ())
updateByPrimaryKey (Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog {..}) = do {_now <- getCurrentTime;
                                                                                       updateWithKV [Se.Set Beam.automationStatus automationStatus,
                                                                                                     Se.Set Beam.nyRegularSubscriptionId (Kernel.Types.Id.getId nyRegularSubscriptionId),
                                                                                                     Se.Set Beam.scheduledPickupTime scheduledPickupTime,
                                                                                                     Se.Set Beam.updatedAt _now,
                                                                                                     Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
                                                                                                     Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)] [Se.And [Se.Is Beam.instanceTransactionId $ Se.Eq instanceTransactionId]]}



instance FromTType' Beam.NyRegularInstanceLog Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog
    where fromTType' (Beam.NyRegularInstanceLogT {..}) = do pure $ Just Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog{automationStatus = automationStatus,
                                                                                                                               createdAt = createdAt,
                                                                                                                               instanceTransactionId = instanceTransactionId,
                                                                                                                               nyRegularSubscriptionId = Kernel.Types.Id.Id nyRegularSubscriptionId,
                                                                                                                               scheduledPickupTime = scheduledPickupTime,
                                                                                                                               updatedAt = updatedAt,
                                                                                                                               merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                               merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId}
instance ToTType' Beam.NyRegularInstanceLog Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog
    where toTType' (Domain.Types.NyRegularInstanceLog.NyRegularInstanceLog {..}) = do Beam.NyRegularInstanceLogT{Beam.automationStatus = automationStatus,
                                                                                                                 Beam.createdAt = createdAt,
                                                                                                                 Beam.instanceTransactionId = instanceTransactionId,
                                                                                                                 Beam.nyRegularSubscriptionId = Kernel.Types.Id.getId nyRegularSubscriptionId,
                                                                                                                 Beam.scheduledPickupTime = scheduledPickupTime,
                                                                                                                 Beam.updatedAt = updatedAt,
                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                                 Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId}



