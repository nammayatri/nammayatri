{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Message.MessageReport where

import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Message.MessageReport
import qualified Domain.Types.Message.MessageTranslation as MTD
import qualified Domain.Types.Person as P
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (create)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Message.MessageReport as BeamMR
import Storage.Tabular.Message.Instances ()
import qualified Storage.Tabular.Message.Message as M
import Storage.Tabular.Message.MessageReport
import qualified Storage.Tabular.Message.MessageTranslation as MT
import qualified Storage.Tabular.Person as PT

-- createMany :: [MessageReport] -> SqlDB ()
-- createMany = Esq.createMany

createMany :: L.MonadFlow m => [MessageReport] -> m ()
createMany msr = void $ traverse create msr

create :: L.MonadFlow m => MessageReport -> m (MeshResult ())
create messageReport = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainMessageReportToBeam messageReport)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

fullMessage ::
  Language ->
  From
    ( Table MessageReportT
        :& Table M.MessageT
        :& MbTable MT.MessageTranslationT
    )
fullMessage lang =
  table
    @MessageReportT
    `innerJoin` table @M.MessageT
      `Esq.on` ( \(messageReport :& message) ->
                   messageReport ^. MessageReportMessageId ==. message ^. M.MessageTId
               )
    `leftJoin` table @MT.MessageTranslationT
      `Esq.on` ( \(_ :& message :& messageTranslation) ->
                   just (message ^. M.MessageTId) ==. messageTranslation ?. MT.MessageTranslationMessageId
                     &&. messageTranslation ?. MT.MessageTranslationLanguage ==. val (Just lang)
               )

findByDriverIdAndLanguage :: Transactionable m => Id P.Driver -> Language -> Maybe Int -> Maybe Int -> m [(MessageReport, Msg.RawMessage, Maybe MTD.MessageTranslation)]
findByDriverIdAndLanguage driverId language mbLimit mbOffset = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  Esq.findAll $ do
    (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
    where_ $
      messageReport ^. MessageReportDriverId ==. val (toKey $ cast driverId)
    orderBy [desc $ messageReport ^. MessageReportCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return (messageReport, message, mbMessageTranslation)

findByDriverIdMessageIdAndLanguage :: Transactionable m => Id P.Driver -> Id Msg.Message -> Language -> m (Maybe (MessageReport, Msg.RawMessage, Maybe MTD.MessageTranslation))
findByDriverIdMessageIdAndLanguage driverId messageId language = do
  Esq.findOne $ do
    (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
    where_ $
      messageReport ^. MessageReportTId ==. val (toKey (messageId, driverId))
    return (messageReport, message, mbMessageTranslation)

-- findByMessageIdAndDriverId :: Transactionable m => Id Msg.Message -> Id P.Driver -> m (Maybe MessageReport)
-- findByMessageIdAndDriverId messageId driverId =
--   Esq.findOne $ do
--     messageReport <- from $ table @MessageReportT
--     where_ $
--       messageReport ^. MessageReportTId ==. val (toKey (messageId, driverId))
--     return messageReport

findByMessageIdAndDriverId :: L.MonadFlow m => Id Msg.Message -> Id P.Driver -> m (Maybe MessageReport)
findByMessageIdAndDriverId (Id messageId) (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMessageReportToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamMR.messageId $ Se.Eq messageId, Se.Is BeamMR.driverId $ Se.Eq driverId]]
    Nothing -> pure Nothing

findByMessageIdAndStatusWithLimitAndOffset ::
  Transactionable m =>
  Maybe Int ->
  Maybe Int ->
  Id Msg.Message ->
  Maybe DeliveryStatus ->
  m [(MessageReport, P.Person)]
findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset messageId mbDeliveryStatus = do
  findAll $ do
    (messageReport :& person) <-
      from $
        table @MessageReportT
          `innerJoin` table @PT.PersonT
            `Esq.on` ( \(messageReport :& person) ->
                         messageReport ^. MessageReportDriverId ==. person ^. PT.PersonTId
                     )
    where_ $
      messageReport ^. MessageReportMessageId ==. val (toKey messageId)
        &&. if isJust mbDeliveryStatus then messageReport ^. MessageReportDeliveryStatus ==. val (fromMaybe Success mbDeliveryStatus) else val True
    orderBy [desc $ messageReport ^. MessageReportCreatedAt]
    limit limitVal
    offset offsetVal
    return (messageReport, person)
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 20
    offsetVal = maybe 0 fromIntegral mbOffset

getMessageCountByStatus :: Transactionable m => Id Msg.Message -> DeliveryStatus -> m Int
getMessageCountByStatus messageId status =
  mkCount <$> do
    Esq.findAll $ do
      messageReport <- from $ table @MessageReportT
      where_ $
        messageReport ^. MessageReportMessageId ==. val (toKey messageId)
          &&. messageReport ^. MessageReportDeliveryStatus ==. val status
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

getMessageCountByReadStatus :: Transactionable m => Id Msg.Message -> m Int
getMessageCountByReadStatus messageId =
  mkCount <$> do
    Esq.findAll $ do
      messageReport <- from $ table @MessageReportT
      where_ $
        messageReport ^. MessageReportMessageId ==. val (toKey messageId)
          &&. messageReport ^. MessageReportReadStatus
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

-- updateSeenAndReplyByMessageIdAndDriverId :: Id Msg.Message -> Id P.Driver -> Bool -> Maybe Text -> SqlDB ()
-- updateSeenAndReplyByMessageIdAndDriverId messageId driverId readStatus reply = do
--   now <- getCurrentTime
--   Esq.update $ \mr -> do
--     set
--       mr
--       [ MessageReportReadStatus =. val readStatus,
--         MessageReportReply =. val reply,
--         MessageReportUpdatedAt =. val now
--       ]
--     where_ $
--       mr ^. MessageReportMessageId ==. val (toKey messageId)
--         &&. mr ^. MessageReportDriverId ==. val (toKey $ cast driverId)

updateSeenAndReplyByMessageIdAndDriverId :: (L.MonadFlow m, MonadTime m) => Id Msg.Message -> Id P.Driver -> Bool -> Maybe Text -> m (MeshResult ())
updateSeenAndReplyByMessageIdAndDriverId messageId driverId readStatus reply = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamMR.readStatus readStatus,
          Se.Set BeamMR.reply reply,
          Se.Set BeamMR.updatedAt now
        ]
        [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateMessageLikeByMessageIdAndDriverIdAndReadStatus :: Id Msg.Message -> Id P.Driver -> SqlDB ()
-- updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId driverId = do
--   now <- getCurrentTime
--   Esq.update $ \mr -> do
--     set
--       mr
--       [ MessageReportLikeStatus =. not_ (mr ^. MessageReportLikeStatus),
--         MessageReportUpdatedAt =. val now
--       ]
--     where_ $
--       mr ^. MessageReportMessageId ==. val (toKey messageId)
--         &&. mr ^. MessageReportDriverId ==. val (toKey $ cast driverId)
--         &&. mr ^. MessageReportReadStatus ==. val True

updateMessageLikeByMessageIdAndDriverIdAndReadStatus :: (L.MonadFlow m, MonadTime m) => Id Msg.Message -> Id P.Driver -> m ()
updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId driverId = do
  messageReport <- findByMessageIdAndDriverId messageId driverId
  case messageReport of
    Just report -> do
      let likeStatus = not report.likeStatus
      dbConf <- L.getOption Extra.EulerPsqlDbCfg
      now <- getCurrentTime
      case dbConf of
        Just dbConf' ->
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              Mesh.meshConfig
              [ Se.Set BeamMR.likeStatus likeStatus,
                Se.Set BeamMR.updatedAt now
              ]
              [Se.And [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId, Se.Is BeamMR.readStatus $ Se.Eq True]]
        Nothing -> pure ()
    Nothing -> pure ()

-- updateDeliveryStatusByMessageIdAndDriverId :: Id Msg.Message -> Id P.Driver -> DeliveryStatus -> SqlDB ()
-- updateDeliveryStatusByMessageIdAndDriverId messageId driverId deliveryStatus = do
--   now <- getCurrentTime
--   Esq.update $ \mr -> do
--     set
--       mr
--       [ MessageReportDeliveryStatus =. val deliveryStatus,
--         MessageReportUpdatedAt =. val now
--       ]
--     where_ $
--       mr ^. MessageReportMessageId ==. val (toKey messageId)
--         &&. mr ^. MessageReportDriverId ==. val (toKey $ cast driverId)

updateDeliveryStatusByMessageIdAndDriverId :: (L.MonadFlow m, MonadTime m) => Id Msg.Message -> Id P.Driver -> DeliveryStatus -> m (MeshResult ())
updateDeliveryStatusByMessageIdAndDriverId messageId driverId deliveryStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamMR.deliveryStatus deliveryStatus,
          Se.Set BeamMR.updatedAt now
        ]
        [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- deleteByPersonId :: Id P.Person -> SqlDB ()
-- deleteByPersonId personId =
--   Esq.delete $ do
--     messagereport <- from $ table @MessageReportT
--     where_ $ messagereport ^. MessageReportDriverId ==. val (toKey personId)

deleteByPersonId :: L.MonadFlow m => Id P.Person -> m ()
deleteByPersonId (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamMR.driverId (Se.Eq personId)]
    Nothing -> pure ()

transformBeamMessageReportToDomain :: BeamMR.MessageReport -> MessageReport
transformBeamMessageReportToDomain BeamMR.MessageReportT {..} = do
  MessageReport
    { messageId = Id messageId,
      driverId = Id driverId,
      deliveryStatus = deliveryStatus,
      readStatus = readStatus,
      likeStatus = likeStatus,
      reply = reply,
      messageDynamicFields = messageDynamicFields,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainMessageReportToBeam :: MessageReport -> BeamMR.MessageReport
transformDomainMessageReportToBeam MessageReport {..} =
  BeamMR.MessageReportT
    { BeamMR.messageId = getId messageId,
      BeamMR.driverId = getId driverId,
      BeamMR.deliveryStatus = deliveryStatus,
      BeamMR.readStatus = readStatus,
      BeamMR.likeStatus = likeStatus,
      BeamMR.reply = reply,
      BeamMR.messageDynamicFields = messageDynamicFields,
      BeamMR.createdAt = createdAt,
      BeamMR.updatedAt = updatedAt
    }
