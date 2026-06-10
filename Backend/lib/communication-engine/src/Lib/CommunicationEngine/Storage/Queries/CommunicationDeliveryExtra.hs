{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.CommunicationEngine.Storage.Queries.CommunicationDeliveryExtra where

import Data.Time (Day, UTCTime (..), addDays)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.CommunicationEngine.Domain.Types.Communication as CommDomain
import qualified Lib.CommunicationEngine.Domain.Types.CommunicationDelivery as CommDelDomain
import qualified Lib.CommunicationEngine.Storage.Beam.BeamFlow
import qualified Lib.CommunicationEngine.Storage.Beam.CommunicationDelivery as Beam
import Lib.CommunicationEngine.Storage.Queries.OrphanInstances.CommunicationDelivery
import qualified Sequelize as Se

findByRecipientIdWithChannel ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Text ->
  Maybe CommDomain.ChannelType ->
  Maybe Int ->
  Maybe Int ->
  Maybe Day ->
  Maybe Day ->
  m [CommDelDomain.CommunicationDelivery]
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
        [Se.And $ [Se.Is Beam.recipientId $ Se.Eq recipientId] <> dateFilters]
        (Se.Desc Beam.createdAt)
        (Just limitVal)
        (Just offsetVal)
    Just channel ->
      findAllWithOptionsKV
        [ Se.And $
            [ Se.Is Beam.recipientId $ Se.Eq recipientId,
              Se.Is Beam.channel $ Se.Eq channel
            ]
              <> dateFilters
        ]
        (Se.Desc Beam.createdAt)
        (Just limitVal)
        (Just offsetVal)

findByCommunicationIdWithFilters ::
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id CommDomain.Communication ->
  Maybe CommDomain.ChannelType ->
  Maybe CommDelDomain.DeliveryStatus ->
  Maybe Int ->
  Maybe Int ->
  m [CommDelDomain.CommunicationDelivery]
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
  (Lib.CommunicationEngine.Storage.Beam.BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id CommDomain.Communication ->
  m ()
deleteByCommunicationId commId =
  deleteWithKV [Se.Is Beam.communicationId $ Se.Eq (Kernel.Types.Id.getId commId)]
