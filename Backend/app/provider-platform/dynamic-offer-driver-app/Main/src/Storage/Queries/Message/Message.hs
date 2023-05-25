{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Message.Message where

import Domain.Types.Merchant (Merchant)
import Domain.Types.Message.Message
import Domain.Types.Message.MessageTranslation as DomainMT
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Message.Message as BeamM
import qualified Storage.Queries.Message.MessageTranslation as MT
import Storage.Tabular.Message.Instances ()
import Storage.Tabular.Message.Message

create :: Message -> SqlDB ()
create msg = Esq.runTransaction $
  withFullEntity msg $ \(message, messageTranslations) -> do
    Esq.create' message
    traverse_ Esq.create' messageTranslations

-- findById :: Transactionable m => Id Message -> m (Maybe RawMessage)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id Message -> m (Maybe RawMessage)
findById (Id messageId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' Mesh.meshConfig [Se.Is BeamM.id $ Se.Eq messageId]
      case result of
        Right msg -> do
          msg' <- traverse transformBeamMessageToDomain msg
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
                    mediaFiles = mediaFiles,
                    merchantId = merchantId,
                    createdAt = createdAt
                  }
            )
              <$> msg'
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findAllWithLimitOffset ::
--   Transactionable m =>
--   Maybe Int ->
--   Maybe Int ->
--   Id Merchant ->
--   m [RawMessage]
-- findAllWithLimitOffset mbLimit mbOffset merchantId = do
--   findAll $ do
--     message <-
--       from $
--         table @MessageT
--     where_ $
--       message ^. MessageMerchantId ==. val (toKey merchantId)
--     orderBy [desc $ message ^. MessageCreatedAt]
--     limit limitVal
--     offset offsetVal
--     return message
--   where
--     limitVal = min (maybe 10 fromIntegral mbLimit) 10
--     offsetVal = maybe 0 fromIntegral mbOffset

findAllWithLimitOffset :: L.MonadFlow m => Maybe Int -> Maybe Int -> Id Merchant -> m [RawMessage]
findAllWithLimitOffset mbLimit mbOffset merchantIdParam = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      srsz <- KV.findAllWithOptionsKVConnector dbConf' Mesh.meshConfig [Se.Is BeamM.merchantId $ Se.Eq (getId merchantIdParam)] (Se.Asc BeamM.createdAt) (Just limitVal) (Just offsetVal)
      case srsz of
        Left _ -> pure []
        Right x -> do
          msg <- traverse transformBeamMessageToDomain x
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
                      mediaFiles = mediaFiles,
                      merchantId = merchantId,
                      createdAt = createdAt
                    }
              )
              msg
    Nothing -> pure []
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset

updateMessageLikeCount :: Id Message -> Int -> SqlDB ()
updateMessageLikeCount messageId value = do
  Esq.update $ \msg -> do
    set msg [MessageLikeCount =. (msg ^. MessageLikeCount) +. val value]
    where_ $ msg ^. MessageId ==. val (getId messageId)

-- updateMessageLikeCount' :: L.MonadFlow m =>  Id Message -> Int-> m (MeshResult ())
-- updateMessageLikeCount' messageId value  = do
--   dbConf <- L.getOption Extra.EulerPsqlDbCfg
--   case dbConf of
--     Just dbConf' ->
--       KV.updateWoReturningWithKVConnector
--         dbConf'
--         Mesh.meshConfig
--         [Se.Set BeamM.likeCount $ BeamM.likeCount + value ]
--         [Se.Is BeamM.id (Se.Eq $ getId messageId)]
--     Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamMessageToDomain :: L.MonadFlow m => BeamM.Message -> m (Message)
transformBeamMessageToDomain BeamM.MessageT {..} = do
  mT' <- MT.findByMessageId (Id id)
  -- let mT = MessageTranslation <*> (language <$> mT') <$> (title <$> mT') <$> (description <$> mT') <$> (shortDescription <$> mT') <$> (label <$> mT') <$> (createdAt <$> mT')
  let mT = (\(DomainMT.MessageTranslation _ language_ title_ label_ description_ shortDescription_ createdAt_) -> Domain.Types.Message.Message.MessageTranslation language_ title_ description_ shortDescription_ label_ createdAt_) <$> mT'
  pure
    Message
      { id = Id id,
        _type = messageType,
        title = title,
        description = description,
        shortDescription = shortDescription,
        label = label,
        likeCount = likeCount,
        mediaFiles = Id <$> mediaFiles,
        messageTranslations = mT,
        merchantId = Id merchantId,
        createdAt = createdAt
      }

transformDomainMessageToBeam :: Message -> BeamM.Message
transformDomainMessageToBeam Message {..} =
  BeamM.MessageT
    { BeamM.id = getId id,
      BeamM.messageType = _type,
      BeamM.title = title,
      BeamM.description = description,
      BeamM.shortDescription = shortDescription,
      BeamM.label = label,
      BeamM.likeCount = likeCount,
      BeamM.mediaFiles = getId <$> mediaFiles,
      BeamM.merchantId = getId merchantId,
      BeamM.createdAt = createdAt
    }
