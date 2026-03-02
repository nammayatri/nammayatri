{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CommunicationExtra where

import qualified Data.Aeson as Aeson
import qualified Domain.Types.Communication
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Communication as Beam
import Storage.Queries.OrphanInstances.Communication

findBySenderIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Maybe Domain.Types.Communication.CommunicationStatus ->
  Maybe Domain.Types.Communication.CommunicationDomain ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.Types.Communication.Communication]
findBySenderIdWithLimitOffset senderId mbStatus mbDomain mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.senderId $ Se.Eq (Kernel.Types.Id.getId senderId)]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
          <> maybe [] (\d -> [Se.Is Beam.domain $ Se.Eq d]) mbDomain
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

updateCommunication ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Communication.Communication ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Domain.Types.Communication.CommunicationContentType ->
  Maybe [Domain.Types.Communication.ChannelType] ->
  Maybe Aeson.Value ->
  m ()
updateCommunication commId mbTitle mbBody mbHtmlBody mbContentType mbChannels mbMediaUrls = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set Beam.updatedAt now]
        <> maybe [] (\t -> [Se.Set Beam.title t]) mbTitle
        <> maybe [] (\b -> [Se.Set Beam.body b]) mbBody
        <> maybe [] (\h -> [Se.Set Beam.htmlBody (Just h)]) mbHtmlBody
        <> maybe [] (\c -> [Se.Set Beam.contentType c]) mbContentType
        <> maybe [] (\ch -> [Se.Set Beam.channels (Just $ Aeson.toJSON ch)]) mbChannels
        <> maybe [] (\m -> [Se.Set Beam.mediaUrls (Just m)]) mbMediaUrls
    )
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId commId)]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Communication.Communication -> m ()
deleteById commId = deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId commId)]
