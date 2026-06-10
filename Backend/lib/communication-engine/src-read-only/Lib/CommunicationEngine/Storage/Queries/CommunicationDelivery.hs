{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.CommunicationEngine.Storage.Queries.CommunicationDelivery (module Lib.CommunicationEngine.Storage.Queries.CommunicationDelivery, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.CommunicationEngine.Domain.Types.Communication
import qualified Lib.CommunicationEngine.Domain.Types.CommunicationDelivery
import qualified Lib.CommunicationEngine.Storage.Beam.BeamFlow
import qualified Lib.CommunicationEngine.Storage.Beam.CommunicationDelivery as Beam
import Lib.CommunicationEngine.Storage.Queries.CommunicationDeliveryExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery -> m ())
create = createWithKV

createMany :: (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery] -> m ())
createMany = traverse_ create

findByCommunicationId ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.Communication.Communication -> m ([Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery]))
findByCommunicationId communicationId = do findAllWithKV [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId communicationId)]

findById ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery -> m (Maybe Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRecipientIdAndChannel ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Lib.CommunicationEngine.Domain.Types.Communication.ChannelType -> m ([Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery]))
findByRecipientIdAndChannel recipientId channel = do findAllWithKV [Se.And [Se.Is Beam.recipientId $ Se.Eq recipientId, Se.Is Beam.channel $ Se.Eq channel]]

updateStatusAndReadAt ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.DeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.Communication.Communication -> Lib.CommunicationEngine.Domain.Types.Communication.ChannelType -> m ())
updateStatusAndReadAt status readAt recipientId communicationId channel = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.status status, Se.Set Beam.readAt readAt, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.recipientId $ Se.Eq recipientId,
          Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId communicationId),
          Se.Is Beam.channel $ Se.Eq channel
        ]
    ]

updateStatusById ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.DeliveryStatus -> Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery -> m (Maybe Lib.CommunicationEngine.Domain.Types.CommunicationDelivery.CommunicationDelivery))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
