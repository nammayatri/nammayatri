{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Message.Message where

import qualified Data.Time as T
import Domain.Types.Merchant (Merchant)
import Domain.Types.Message.Message
import Domain.Types.Message.MessageTranslation as DomainMT
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Message.Message as BeamM
import qualified Storage.Queries.Message.MessageTranslation as MT

createMessage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Message -> m ()
createMessage msg = do
  let mT = fmap (fn msg.id) (msg.messageTranslations)
      fn id' (Domain.Types.Message.Message.MessageTranslation language_ title_ description_ shortDescription_ label_ createdAt_) = DomainMT.MessageTranslation id' language_ title_ label_ description_ shortDescription_ createdAt_
  MT.createMany mT >> createWithKV msg

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Message -> m ()
create = createWithKV

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> m (Maybe RawMessage)
findById (Id messageId) = do
  message <- findOneWithKV [Se.Is BeamM.id $ Se.Eq messageId]
  now <- getCurrentTime
  pure $
    ( \Message {..} ->
        RawMessage
          { id = id,
            _type = _type,
            title = title,
            description = description,
            shortDescription = shortDescription,
            label = label,
            likeCount = likeCount,
            viewCount = viewCount,
            mediaFiles = mediaFiles,
            merchantId = merchantId,
            createdAt = createdAt,
            sentAt = Just now
          }
    )
      <$> message

findAllWithLimitOffset :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> Maybe Int -> Id Merchant -> m [RawMessage]
findAllWithLimitOffset mbLimit mbOffset merchantIdParam = do
  messages <- findAllWithOptionsDb [Se.Is BeamM.merchantId $ Se.Eq (getId merchantIdParam)] (Se.Desc BeamM.createdAt) (Just limitVal) (Just offsetVal)
  now <- getCurrentTime
  pure $
    map
      ( \Message {..} ->
          RawMessage
            { id = id,
              _type = _type,
              title = title,
              description = description,
              shortDescription = shortDescription,
              label = label,
              likeCount = likeCount,
              viewCount = viewCount,
              mediaFiles = mediaFiles,
              merchantId = merchantId,
              createdAt = createdAt,
              sentAt = Just now
            }
      )
      messages
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset

updateMessageLikeCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> Int -> m ()
updateMessageLikeCount messageId value = do
  findById messageId >>= \case
    Nothing -> pure ()
    Just msg -> do
      let likeCount = msg.likeCount
      updateOneWithKV
        [Se.Set BeamM.likeCount $ likeCount + value]
        [Se.Is BeamM.id (Se.Eq $ getId messageId)]

updateMessageViewCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Message -> Int -> m ()
updateMessageViewCount messageId value = do
  findById messageId >>= \case
    Just msg -> do
      let viewCount = msg.viewCount
      updateOneWithKV
        [Se.Set BeamM.viewCount $ viewCount + value]
        [Se.Is BeamM.id (Se.Eq $ getId messageId)]
    Nothing -> pure ()

instance FromTType' BeamM.Message Message where
  fromTType' BeamM.MessageT {..} = do
    mT' <- MT.findByMessageId (Id id)
    let mT = (\(DomainMT.MessageTranslation _ language_ title_ label_ description_ shortDescription_ createdAt_) -> Domain.Types.Message.Message.MessageTranslation language_ title_ description_ shortDescription_ label_ createdAt_) <$> mT'
    pure $
      Just
        Message
          { id = Id id,
            _type = messageType,
            title = title,
            description = description,
            shortDescription = shortDescription,
            label = label,
            likeCount = likeCount,
            viewCount = viewCount,
            mediaFiles = Id <$> mediaFiles,
            messageTranslations = mT,
            merchantId = Id merchantId,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamM.Message Message where
  toTType' Message {..} = do
    BeamM.MessageT
      { BeamM.id = getId id,
        BeamM.messageType = _type,
        BeamM.title = title,
        BeamM.description = description,
        BeamM.shortDescription = shortDescription,
        BeamM.label = label,
        BeamM.likeCount = likeCount,
        BeamM.viewCount = viewCount,
        BeamM.mediaFiles = getId <$> mediaFiles,
        BeamM.merchantId = getId merchantId,
        BeamM.createdAt = T.utcToLocalTime T.utc createdAt
      }
