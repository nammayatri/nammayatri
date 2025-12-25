module Storage.Queries.MessageReportExtra where

import qualified Data.Time as T
import qualified Database.Beam as B
import Database.Beam.Postgres
import Domain.Types.Message
import qualified Domain.Types.Message as Msg (Message)
import Domain.Types.MessageReport as DTMR
import qualified Domain.Types.MessageTranslation as MTD
import qualified Domain.Types.Person as P
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Message as BeamM
import qualified Storage.Beam.MessageReport as BeamMR
import qualified Storage.Beam.MessageTranslation as BeamMT
import Storage.Queries.Message as QMM hiding (create)
import qualified Storage.Queries.Message ()
import Storage.Queries.MessageTranslation as QMMT hiding (create)
import Storage.Queries.OrphanInstances.MessageReport ()
import qualified Storage.Queries.Person ()

-- Extra code goes here --

findByDriverIdAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id P.Driver -> Language -> Maybe Int -> Maybe Int -> m [(MessageReport, RawMessage, Maybe MTD.MessageTranslation)]
findByDriverIdAndLanguage driverId language mbLimit mbOffset = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  result <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limitVal) $
          B.offset_ (fromIntegral offsetVal) $
            B.orderBy_ (\(messageReport', _, _) -> B.desc_ (messageReport'.createdAt)) $
              B.filter_'
                (\(messageReport', _, _) -> messageReport'.driverId B.==?. B.val_ (getId driverId))
                do
                  messageReport <- B.all_ (BeamCommon.messageReport BeamCommon.atlasDB)
                  message <- B.join_' (BeamCommon.message BeamCommon.atlasDB) (\message' -> BeamMR.messageId messageReport B.==?. BeamM.id message')
                  messageTranslation <- B.leftJoin_' (B.all_ $ BeamCommon.messageTranslation BeamCommon.atlasDB) (\messageTranslation' -> BeamM.id message B.==?. BeamMT.messageId messageTranslation' B.&&?. BeamMT.language messageTranslation' B.==?. B.val_ language)
                  pure (messageReport, message, messageTranslation)
  case result of
    Right x -> do
      let messageReport = fmap fst' x
          message = fmap snd' x
          messageTranslation = fmap thd' x
      p <- catMaybes <$> mapM fromTType' messageReport
      di <- catMaybes <$> mapM fromTType' message
      v <- mapM (maybe (pure Nothing) fromTType') messageTranslation
      let rawMessageFromMessage Message {..} =
            RawMessage
              { id = id,
                _type = _type,
                title = title,
                description = description,
                shortDescription = shortDescription,
                label = label,
                likeCount = likeCount,
                viewCount = viewCount,
                shareable,
                alwaysTriggerOnOnboarding = alwaysTriggerOnOnboarding,
                mediaFiles = mediaFiles,
                merchantId = merchantId,
                createdAt = createdAt
              }
      pure $ zip3 p (rawMessageFromMessage <$> di) v
    Left _ -> pure []
  where
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    thd' (_, _, z) = z

findAllMessageWithSeConditionCreatedAtdesc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamM.MessageT] -> m [Message]
findAllMessageWithSeConditionCreatedAtdesc conditions = findAllWithOptionsDb conditions (Se.Desc BeamM.createdAt) Nothing Nothing

findAllMessageTranslationWithSeConditionCreatedAtdesc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamMT.MessageTranslationT] -> m [MTD.MessageTranslation]
findAllMessageTranslationWithSeConditionCreatedAtdesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamMT.createdAt) Nothing Nothing

findByDriverIdMessageIdAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id P.Driver -> Id Msg.Message -> Language -> m (Maybe (MessageReport, RawMessage, Maybe MTD.MessageTranslation))
findByDriverIdMessageIdAndLanguage driverId messageId language = do
  maybeMessageReport <- findByMessageIdAndDriverId messageId driverId
  maybeRawMessage <- QMM.findById messageId
  maybeMessageTranslation <- QMMT.findByMessageIdAndLanguage messageId language
  case (maybeMessageReport, maybeRawMessage) of
    (Just messageReport, Just rawMessage) ->
      pure $ Just (messageReport, rawMessage, maybeMessageTranslation)
    _ ->
      pure Nothing

findByMessageIdAndStatusWithLimitAndOffset :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> Maybe Int -> Id Msg.Message -> Maybe DeliveryStatus -> m [(MessageReport, P.Person)]
findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset (Id messageId) mbDeliveryStatus = do
  dbConf <- getReplicaBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\(messageReport, _) -> B.desc_ (messageReport.createdAt)) $
                B.filter_'
                  ( \(messageReport, _) ->
                      messageReport.messageId B.==?. B.val_ messageId
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\deliveryStatus -> messageReport.deliveryStatus B.==?. B.val_ deliveryStatus) mbDeliveryStatus
                  )
                  do
                    messageReport <- B.all_ (BeamCommon.messageReport BeamCommon.atlasDB)
                    person <- B.join_' (BeamCommon.person BeamCommon.atlasDB) (\person -> person.id B.==?. messageReport.driverId)
                    pure (messageReport, person)
  case resp of
    Right resp' -> do
      let messageReport' = fst <$> resp'
          person' = snd <$> resp'
      messageReport <- catMaybes <$> mapM fromTType' messageReport'
      person <- catMaybes <$> mapM fromTType' person'
      pure $ zip messageReport person
    Left _ -> pure []
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 20
    offsetVal = maybe 0 fromIntegral mbOffset

getMessageCountByStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> DeliveryStatus -> m Int
getMessageCountByStatus (Id messageID) status = do
  dbConf <- getReplicaBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_' (\(BeamMR.MessageReportT {..}) -> messageId B.==?. B.val_ messageID B.&&?. (deliveryStatus B.==?. B.val_ status)) $
              B.all_ (BeamCommon.messageReport BeamCommon.atlasDB)
  pure (either (const 0) (fromMaybe 0) resp)

getMessageCountByReadStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> m Int
getMessageCountByReadStatus (Id messageID) = do
  dbConf <- getReplicaBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_' (\(BeamMR.MessageReportT {..}) -> messageId B.==?. B.val_ messageID B.&&?. readStatus B.==?. B.val_ True) $
              B.all_ (BeamCommon.messageReport BeamCommon.atlasDB)
  pure (either (const 0) (fromMaybe 0) resp)

updateMessageLikeByMessageIdAndDriverIdAndReadStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> Id P.Driver -> m ()
updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId driverId = do
  findByMessageIdAndDriverId messageId driverId >>= \case
    Just report -> do
      let likeStatus = not report.likeStatus
      now <- getCurrentTime
      updateOneWithKV
        [ Se.Set BeamMR.likeStatus likeStatus,
          Se.Set BeamMR.updatedAt $ T.utcToLocalTime T.utc now
        ]
        [Se.And [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId, Se.Is BeamMR.readStatus $ Se.Eq True]]
    Nothing -> pure ()

findByMessageIdAndDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> Id P.Driver -> m (Maybe MessageReport)
findByMessageIdAndDriverId (Id messageId) (Id driverId) = findOneWithKV [Se.And [Se.Is BeamMR.messageId $ Se.Eq messageId, Se.Is BeamMR.driverId $ Se.Eq driverId]]

updateSeenAndReplyByMessageIdAndDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> Id P.Driver -> Bool -> Maybe Text -> m ()
updateSeenAndReplyByMessageIdAndDriverId messageId driverId readStatus reply = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamMR.readStatus readStatus,
      Se.Set BeamMR.reply reply,
      Se.Set BeamMR.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Se.And [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]]

updateDeliveryStatusByMessageIdAndDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Msg.Message -> Id P.Driver -> DeliveryStatus -> m ()
updateDeliveryStatusByMessageIdAndDriverId messageId driverId deliveryStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamMR.deliveryStatus deliveryStatus,
      Se.Set BeamMR.updatedAt $ T.utcToLocalTime T.utc now
    ]
    [Se.And [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]]
