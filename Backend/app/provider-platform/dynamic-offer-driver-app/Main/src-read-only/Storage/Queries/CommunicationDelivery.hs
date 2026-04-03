{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.CommunicationDelivery (module Storage.Queries.CommunicationDelivery, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.CommunicationDeliveryExtra as ReExport
import qualified Domain.Types.CommunicationDelivery
import qualified Storage.Beam.CommunicationDelivery as Beam
import qualified Domain.Types.Communication
import qualified Kernel.Types.Id
import qualified Kernel.Prelude
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CommunicationDelivery.CommunicationDelivery -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CommunicationDelivery.CommunicationDelivery] -> m ())
createMany = traverse_ create
findByCommunicationId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Communication.Communication -> m ([Domain.Types.CommunicationDelivery.CommunicationDelivery]))
findByCommunicationId communicationId = do findAllWithKV [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId communicationId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
            (Kernel.Types.Id.Id Domain.Types.CommunicationDelivery.CommunicationDelivery -> m (Maybe Domain.Types.CommunicationDelivery.CommunicationDelivery))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByRecipientIdAndChannel :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                               (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Communication.ChannelType -> m ([Domain.Types.CommunicationDelivery.CommunicationDelivery]))
findByRecipientIdAndChannel recipientId channel = do findAllWithKV [Se.And [Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId), Se.Is Beam.channel $ Se.Eq channel]]
updateStatusAndReadAt :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                         (Domain.Types.CommunicationDelivery.DeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Communication.Communication -> Domain.Types.Communication.ChannelType -> m ())
updateStatusAndReadAt status readAt recipientId communicationId channel = do {_now <- getCurrentTime;
                                                                              updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.readAt readAt, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId),
                                                                                                                                                                                          Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId communicationId),
                                                                                                                                                                                          Se.Is Beam.channel $ Se.Eq channel]]}
updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Domain.Types.CommunicationDelivery.DeliveryStatus -> Kernel.Types.Id.Id Domain.Types.CommunicationDelivery.CommunicationDelivery -> m ())
updateStatusById status id = do {_now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.CommunicationDelivery.CommunicationDelivery -> m (Maybe Domain.Types.CommunicationDelivery.CommunicationDelivery))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



