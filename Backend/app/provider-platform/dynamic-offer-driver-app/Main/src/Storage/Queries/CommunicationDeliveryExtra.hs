{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CommunicationDeliveryExtra where

import qualified Domain.Types.Communication
import qualified Domain.Types.CommunicationDelivery
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CommunicationDelivery as Beam
import qualified Storage.Queries.CommunicationExtra as QComm
import Storage.Queries.OrphanInstances.CommunicationDelivery

updateFailureReasonAndStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.CommunicationDelivery.CommunicationDelivery ->
  Domain.Types.CommunicationDelivery.DeliveryStatus ->
  Maybe Text ->
  m ()
updateFailureReasonAndStatusById deliveryId status mbReason = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set Beam.status status, Se.Set Beam.updatedAt now]
        <> maybe [] (\r -> [Se.Set Beam.failureReason (Just r)]) mbReason
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId deliveryId)]

findByRecipientIdCommunicationIdAndChannel ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
  Domain.Types.Communication.ChannelType ->
  m (Maybe Domain.Types.CommunicationDelivery.CommunicationDelivery)
findByRecipientIdCommunicationIdAndChannel recipientId communicationId channel =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId),
          Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId communicationId),
          Se.Is Beam.channel $ Se.Eq channel
        ]
    ]

findByRecipientIdAndWebChannel ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.CommunicationDelivery.CommunicationDelivery]
findByRecipientIdAndWebChannel recipientId mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId),
          Se.Is Beam.channel $ Se.Eq Domain.Types.Communication.CH_WEB
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findByCommunicationIdWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
  Maybe Domain.Types.Communication.ChannelType ->
  Maybe Domain.Types.CommunicationDelivery.DeliveryStatus ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.CommunicationDelivery.CommunicationDelivery]
findByCommunicationIdWithFilters commId mbChannel mbStatus mbLimit mbOffset = do
  let limitVal = min 100 $ fromMaybe 20 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId commId)]
          <> maybe [] (\ch -> [Se.Is Beam.channel $ Se.Eq ch]) mbChannel
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findByRecipientIdAndWebChannelWithDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.Communication.CommunicationDomain ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.CommunicationDelivery.CommunicationDelivery]
findByRecipientIdAndWebChannelWithDomain recipientId mbDomain mbLimit mbOffset =
  case mbDomain of
    Nothing -> findByRecipientIdAndWebChannel recipientId mbLimit mbOffset
    Just domain -> do
      let limitVal = min 50 $ fromMaybe 10 mbLimit
          offsetVal = fromMaybe 0 mbOffset
      commIds <- QComm.findIdsByDomainWithLimitOffset domain (Just limitVal) (Just offsetVal)
      findAllWithOptionsKV
        [ Se.And
            [ Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId),
              Se.Is Beam.channel $ Se.Eq Domain.Types.Communication.CH_WEB,
              Se.Is Beam.communicationId $ Se.In (map Kernel.Types.Id.getId commIds)
            ]
        ]
        (Se.Desc Beam.createdAt)
        (Just limitVal)
        (Just offsetVal)

deleteByCommunicationId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
  m ()
deleteByCommunicationId commId =
  deleteWithKV [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId commId)]
