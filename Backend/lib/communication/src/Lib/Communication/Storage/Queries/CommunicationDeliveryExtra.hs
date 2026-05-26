{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Communication.Storage.Queries.CommunicationDeliveryExtra where

import qualified Lib.Communication.Domain.Types.Communication
import qualified Lib.Communication.Domain.Types.CommunicationDelivery
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Lib.Communication.Storage.Beam.CommunicationDelivery as Beam
import Lib.Communication.Storage.Queries.OrphanInstances.CommunicationDelivery ()

findByRecipientIdAndWebChannel ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Maybe Int ->
  Maybe Int ->
  m [Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationDelivery]
findByRecipientIdAndWebChannel recipientId mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.recipientId $ Se.Eq recipientId,
          Se.Is Beam.channel $ Se.Eq Lib.Communication.Domain.Types.Communication.CH_WEB
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

findByCommunicationIdWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Lib.Communication.Domain.Types.Communication.Communication ->
  Maybe Lib.Communication.Domain.Types.Communication.ChannelType ->
  Maybe Lib.Communication.Domain.Types.CommunicationDelivery.DeliveryStatus ->
  Maybe Int ->
  Maybe Int ->
  m [Lib.Communication.Domain.Types.CommunicationDelivery.CommunicationDelivery]
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

deleteByCommunicationId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Lib.Communication.Domain.Types.Communication.Communication ->
  m ()
deleteByCommunicationId commId =
  deleteWithKV [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId commId)]
