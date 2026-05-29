{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CommunicationDeliveryExtra where

import Data.Time (Day, UTCTime (..), addDays)
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
import Storage.Queries.OrphanInstances.CommunicationDelivery

findByRecipientIdWithChannel ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.Communication.ChannelType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Day ->
  Maybe Day ->
  m [Domain.Types.CommunicationDelivery.CommunicationDelivery]
findByRecipientIdWithChannel recipientId mbChannel mbLimit mbOffset mbFromDate mbToDate = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
      dateFilters = case (mbFromDate, mbToDate) of
        (Just fromDate, Just toDate) ->
          [ Se.Is Beam.createdAt $ Se.GreaterThanOrEq (UTCTime fromDate 0),
            Se.Is Beam.createdAt $ Se.LessThan (UTCTime (addDays 1 toDate) 0)
          ]
        (Just fromDate, Nothing) ->
          [Se.Is Beam.createdAt $ Se.GreaterThanOrEq (UTCTime fromDate 0)]
        (Nothing, Just toDate) ->
          [Se.Is Beam.createdAt $ Se.LessThan (UTCTime (addDays 1 toDate) 0)]
        _ -> []
  case mbChannel of
    Nothing ->
      findAllWithOptionsKV
        [Se.And $ [Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId)] <> dateFilters]
        (Se.Desc Beam.createdAt)
        (Just limitVal)
        (Just offsetVal)
    Just channel ->
      findAllWithOptionsKV
        [ Se.And $
            [ Se.Is Beam.recipientId $ Se.Eq (Kernel.Types.Id.getId recipientId),
              Se.Is Beam.channel $ Se.Eq channel
            ]
              <> dateFilters
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

deleteByCommunicationId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
  m ()
deleteByCommunicationId commId =
  deleteWithKV [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId commId)]
