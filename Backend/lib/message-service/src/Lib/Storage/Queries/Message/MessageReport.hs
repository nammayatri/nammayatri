{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Lib.Storage.Queries.Message.MessageReport where

import Kernel.External.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import qualified Lib.Domain.Types.Message.Message as Msg
import Lib.Domain.Types.Message.MessageReport
import qualified Lib.Domain.Types.Message.MessageTranslation as MTD
import Lib.Storage.Tabular.Message.Instances ()
import qualified Lib.Storage.Tabular.Message.Message as M
import Lib.Storage.Tabular.Message.MessageReport
import qualified Lib.Storage.Tabular.Message.MessageTranslation as MT

createMany :: [MessageReport] -> SqlDB ()
createMany = Esq.createMany

create :: MessageReport -> SqlDB ()
create = Esq.create

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

findByPersonIdAndLanguage :: Transactionable m => Id Person -> Language -> Maybe Int -> Maybe Int -> m [(MessageReport, Msg.RawMessage, Maybe MTD.MessageTranslation)]
findByPersonIdAndLanguage personId language mbLimit mbOffset = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  Esq.findAll $ do
    (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
    where_ $
      messageReport ^. MessageReportPersonId ==. val personId.getId
    orderBy [desc $ messageReport ^. MessageReportCreatedAt]
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    return (messageReport, message, mbMessageTranslation)

findByPersonIdMessageIdAndLanguage :: Transactionable m => Id Person -> Id Msg.Message -> Language -> m (Maybe (MessageReport, Msg.RawMessage, Maybe MTD.MessageTranslation))
findByPersonIdMessageIdAndLanguage personId messageId language = do
  Esq.findOne $ do
    (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
    where_ $
      messageReport ^. MessageReportTId ==. val (toKey (messageId, personId.getId))
    return (messageReport, message, mbMessageTranslation)

findByMessageIdAndPersonId :: Transactionable m => Id Msg.Message -> Id Person -> m (Maybe MessageReport)
findByMessageIdAndPersonId messageId personId =
  Esq.findOne $ do
    messageReport <- from $ table @MessageReportT
    where_ $
      messageReport ^. MessageReportTId ==. val (toKey (messageId, personId.getId))
    return messageReport

findByMessageIdAndStatusWithLimitAndOffset ::
  Transactionable m =>
  Maybe Int ->
  Maybe Int ->
  Id Msg.Message ->
  Maybe DeliveryStatus ->
  m [MessageReport]
findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset messageId mbDeliveryStatus = do
  findAll $ do
    messageReport <-
      from $
        table @MessageReportT
    where_ $
      messageReport ^. MessageReportMessageId ==. val (toKey messageId)
        &&. if isJust mbDeliveryStatus then messageReport ^. MessageReportDeliveryStatus ==. val (fromMaybe Success mbDeliveryStatus) else val True
    orderBy [desc $ messageReport ^. MessageReportCreatedAt]
    limit limitVal
    offset offsetVal
    return messageReport
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

updateSeenAndReplyByMessageIdAndPersonId :: Id Msg.Message -> Id Person -> Bool -> Maybe Text -> SqlDB ()
updateSeenAndReplyByMessageIdAndPersonId messageId personId readStatus reply = do
  now <- getCurrentTime
  Esq.update $ \mr -> do
    set
      mr
      [ MessageReportReadStatus =. val readStatus,
        MessageReportReply =. val reply,
        MessageReportUpdatedAt =. val now
      ]
    where_ $
      mr ^. MessageReportMessageId ==. val (toKey messageId)
        &&. mr ^. MessageReportPersonId ==. val personId.getId

updateMessageLikeByMessageIdAndPersonIdAndReadStatus :: Id Msg.Message -> Id Person -> SqlDB ()
updateMessageLikeByMessageIdAndPersonIdAndReadStatus messageId personId = do
  now <- getCurrentTime
  Esq.update $ \mr -> do
    set
      mr
      [ MessageReportLikeStatus =. not_ (mr ^. MessageReportLikeStatus),
        MessageReportUpdatedAt =. val now
      ]
    where_ $
      mr ^. MessageReportMessageId ==. val (toKey messageId)
        &&. mr ^. MessageReportPersonId ==. val personId.getId
        &&. mr ^. MessageReportReadStatus ==. val True

updateDeliveryStatusByMessageIdAndPersonId :: Id Msg.Message -> Id Person -> DeliveryStatus -> SqlDB ()
updateDeliveryStatusByMessageIdAndPersonId messageId personId deliveryStatus = do
  now <- getCurrentTime
  Esq.update $ \mr -> do
    set
      mr
      [ MessageReportDeliveryStatus =. val deliveryStatus,
        MessageReportUpdatedAt =. val now
      ]
    where_ $
      mr ^. MessageReportMessageId ==. val (toKey messageId)
        &&. mr ^. MessageReportPersonId ==. val personId.getId

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    messagereport <- from $ table @MessageReportT
    where_ $ messagereport ^. MessageReportPersonId ==. val personId.getId
