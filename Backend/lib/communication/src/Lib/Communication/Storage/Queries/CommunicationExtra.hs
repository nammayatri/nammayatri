{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Communication.Storage.Queries.CommunicationExtra where

import qualified Data.Aeson as Aeson
import qualified Lib.Communication.Domain.Types.Communication
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Lib.Communication.Storage.Beam.Communication as Beam
import Lib.Communication.Storage.Queries.OrphanInstances.Communication ()

findBySenderIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Maybe Lib.Communication.Domain.Types.Communication.CommunicationStatus ->
  Maybe Lib.Communication.Domain.Types.Communication.CommunicationDomain ->
  Maybe Int ->
  Maybe Int ->
  m [Lib.Communication.Domain.Types.Communication.Communication]
findBySenderIdWithLimitOffset senderId mbStatus mbDomain mbLimit mbOffset = do
  let limitVal = min 50 $ fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.senderId $ Se.Eq senderId]
          <> maybe [] (\s -> [Se.Is Beam.status $ Se.Eq s]) mbStatus
          <> maybe [] (\d -> [Se.Is Beam.domain $ Se.Eq d]) mbDomain
    ]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)

updateCommunication ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Lib.Communication.Domain.Types.Communication.Communication ->
  Maybe Kernel.Prelude.Text ->
  Maybe Kernel.Prelude.Text ->
  Maybe Kernel.Prelude.Text ->
  Maybe Lib.Communication.Domain.Types.Communication.CommunicationContentType ->
  Maybe [Lib.Communication.Domain.Types.Communication.ChannelType] ->
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

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Lib.Communication.Domain.Types.Communication.Communication -> m ()
deleteById commId = deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId commId)]
