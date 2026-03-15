module DBSync.DBSync where

import qualified Config.Env as Env
import qualified Constants as C
import Control.Monad.Trans.Except
import DBSync.BatchCreate
import DBSync.Create
import DBSync.Delete
import DBSync.Update
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import Data.Pool (withResource)
import qualified Data.Text as T hiding (elem)
import qualified Data.Text.Encoding as DTE
import Data.Time.Clock hiding (getCurrentTime)
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types as PGS
import qualified Database.Redis as R
import qualified EulerHS.KVConnector.Compression as C
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (fail, id, succ)
import qualified EulerHS.Types as ET
import GHC.Float (int2Double)
import Kafka.Producer as KafkaProd
import qualified Kernel.Beam.Types as KBT
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Text
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Types.DBSync
import qualified Types.Event as Event
import Utils.Config
import Utils.Parse
import Utils.Redis
import qualified Utils.Redis as RQ
import Utils.Utils

peekDBCommand :: Text -> Integer -> Flow (Either ET.KVDBReply (Maybe [(EL.KVDBStreamEntryID, [(Text, ByteString)])]))
peekDBCommand dbStreamKey count = do
  dbReadResponse <-
    ((find (\x -> decodeToText (R.stream x) == dbStreamKey) =<<) <$>)
      <$> RQ.readValueFromRedisStream dbStreamKey (EL.EntryID $ EL.KVDBStreamEntryID 0 0) count
  pure $ parseReadStreams <$> dbReadResponse
  where
    parseReadStreams = (>>= (\(R.XReadResponse _ entries) -> Just $ entryToTuple <$> entries))
    entryToTuple (R.StreamsRecord recordId items) = (parseStreamEntryId recordId, first decodeToText <$> items)

getDecompressedValue :: ByteString -> Flow ByteString
getDecompressedValue val = do
  if C.isCompressionAllowed
    then do
      decompressedObject <- EL.runIO $ C.decompress val
      case decompressedObject of
        Left err -> do
          EL.logError ("DECOMPRESSION_ERROR" :: Text) $ "Error while decompressing " <> show err
          pure val
        Right decompressedObject' -> do
          EL.logDebug ("DECOMPRESSION_RESULT_SUCCESS" :: Text) $ "Original Decompressed value size: " <> show (length decompressedObject') <> " compressed value size: " <> show (length val)
          pure decompressedObject'
    else do
      pure val

parseDBCommand :: Text -> (EL.KVDBStreamEntryID, [(Text, ByteString)]) -> Flow (Maybe (EL.KVDBStreamEntryID, DBCommand, ByteString))
parseDBCommand dbStreamKey entries =
  case entries of
    (id, [("command", val')]) -> do
      val <- getDecompressedValue val'
      case A.eitherDecode $ BL.fromStrict val of
        Right cmd -> do
          pure $ Just (id, cmd, val)
        Left err -> do
          logParseError $ ("Bad entries: " :: Text) <> show err
          void $
            publishDBSyncMetric $
              uncurry Event.ParseDBCommandError $
                bimap
                  (fromMaybe "UNKNOWN_ACTION")
                  (fromMaybe "UNKNOWN_MODEL_OBJECT")
                  (getActionAndModelName val)
          isForcePushEnabled <- fromMaybe False <$> getValueFromRedis C.forceDrainEnabledKey
          if isForcePushEnabled
            then do
              void $ addValueToErrorQueue (T.pack C.ecRedisFailedStream) [("command", val)]
              void $ dropDBCommand dbStreamKey id
            else stopDrainer
          pure Nothing
    (_, cmd) -> do
      logParseError $ "Bad entries: " <> ("Unparsable values in stream" :: Text)
      void $ publishDBSyncMetric $ Event.ParseDBCommandError "UNPARSABLE_OBJECT" "UNPARSABLE_OBJECT"
      isForcePushEnabled <- fromMaybe False <$> getValueFromRedis C.forceDrainEnabledKey
      if isForcePushEnabled
        then EL.logInfo ("UNKNOWN_COMMAND" :: Text) (show cmd)
        else stopDrainer
      pure Nothing
  where
    logParseError err = EL.logError ("PARSE_DB_COMMAND_ERROR" :: Text) $ "Error while parsing " <> err

    getActionAndModelName dbCommandByteString = do
      case A.decode $ BL.fromStrict dbCommandByteString of
        Just _decodedDBCommandObject@(A.Object o) ->
          let mbAction = case AKM.lookup "tag" o of
                Just (A.String actionTag) -> return actionTag
                _ -> Nothing
              mbModel = case AKM.lookup "contents_v2" o of
                Just (A.Object commandObject) -> case AKM.lookup "command" commandObject of
                  Just (A.Object command) -> case AKM.lookup "tag" command of
                    Just (A.String modelTag) -> return modelTag
                    _ -> Nothing
                  _ -> Nothing
                _ -> Nothing
           in (mbAction, mbModel)
        _ -> (Nothing, Nothing)

dropDBCommand :: Text -> EL.KVDBStreamEntryID -> Flow ()
dropDBCommand dbStreamKey entryId = do
  count <- RQ.deleteStreamValue dbStreamKey [entryId]
  case count of
    Right 1 -> pure ()
    Right n -> do
      void $ publishDBSyncMetric Event.DropDBCommandError
      EL.logError ("DROP_DB_COMMAND_ERROR" :: Text) $ ("entryId : " :: Text) <> show entryId <> (", Dropped : " :: Text) <> show n
    Left e -> do
      void $ publishDBSyncMetric Event.DropDBCommandError
      EL.logError ("DROP_DB_COMMAND_ERROR" :: Text) $ ("entryId : " :: Text) <> show entryId <> (", Error : " :: Text) <> show e

runCriticalDBSyncOperations :: Text -> [(EL.KVDBStreamEntryID, ByteString)] -> [(EL.KVDBStreamEntryID, ByteString)] -> [(EL.KVDBStreamEntryID, ByteString)] -> ExceptT Int Flow Int
runCriticalDBSyncOperations dbStreamKey updateEntries deleteEntries createDataEntries = do
  getBatchCreateEnabled <- EL.runIO Env.getBatchCreateEnabled
  isForcePushEnabled <- pureRightExceptT $ fromMaybe False <$> getValueFromRedis C.forceDrainEnabledKey
  -- (cSucc, cFail) <- pureRightExceptT $ executeInSequence runCreate ([], []) dbStreamKey createDataEntries
  (cSucc, cFail) <- pureRightExceptT $ if isForcePushEnabled || not getBatchCreateEnabled then executeInSequence runCreate ([], []) dbStreamKey createDataEntries else executeBatchedCreate dbStreamKey createDataEntries
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "Create" (fromIntegral $ length cSucc)
  when getBatchCreateEnabled $ void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "CreateInBatch" (if null cSucc then 0 else 1)
  void $
    if null cSucc
      then pureRightExceptT $ publishDBSyncMetric $ Event.QueryDrainLatency "Create" 0
      else pureRightExceptT $ traverse_ (publishDrainLatency "Create") cSucc
  void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) cSucc

  void $
    if not (null cFail)
      then do
        dlqMaxRetries <- EL.runIO Env.getDlqMaxRetries
        (recovered, stillFailed) <- pureRightExceptT $ retryFailedWithBackoff runCreate dbStreamKey createDataEntries cFail dlqMaxRetries 1
        void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) recovered
        if not (null stillFailed)
          then do
            pureRightExceptT $ addToDLQ (T.pack C.ecRedisDLQStream) createDataEntries stillFailed "Create"
            pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) stillFailed
            pure (length cSucc + length recovered)
          else pure (length cSucc + length recovered)
      else pure (length cSucc)

  (uSucc, uFail) <- pureRightExceptT $ executeInSequence runUpdate ([], []) dbStreamKey updateEntries
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "Update" (fromIntegral $ length uSucc)
  void $
    if null uSucc
      then pureRightExceptT $ publishDBSyncMetric $ Event.QueryDrainLatency "Update" 0
      else pureRightExceptT $ traverse_ (publishDrainLatency "Update") uSucc
  void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) uSucc

  void $
    if not (null uFail)
      then do
        dlqMaxRetriesU <- EL.runIO Env.getDlqMaxRetries
        (recoveredU, stillFailedU) <- pureRightExceptT $ retryFailedWithBackoff runUpdate dbStreamKey updateEntries uFail dlqMaxRetriesU 1
        void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) recoveredU
        if not (null stillFailedU)
          then do
            pureRightExceptT $ addToDLQ (T.pack C.ecRedisDLQStream) updateEntries stillFailedU "Update"
            pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) stillFailedU
            pure (length cSucc + length uSucc + length recoveredU)
          else pure (length cSucc + length uSucc + length recoveredU)
      else pure (length cSucc + length uSucc)

  (dSucc, dFail) <- pureRightExceptT $ executeInSequence runDelete ([], []) dbStreamKey deleteEntries
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "Delete" (fromIntegral $ length dSucc)
  void $
    if null dSucc
      then pureRightExceptT $ publishDBSyncMetric $ Event.QueryDrainLatency "Delete" 0
      else pureRightExceptT $ traverse_ (publishDrainLatency "Delete") dSucc
  void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) dSucc

  if not (null dFail)
    then do
      dlqMaxRetriesD <- EL.runIO Env.getDlqMaxRetries
      (recoveredD, stillFailedD) <- pureRightExceptT $ retryFailedWithBackoff runDelete dbStreamKey deleteEntries dFail dlqMaxRetriesD 1
      void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) recoveredD
      if not (null stillFailedD)
        then do
          pureRightExceptT $ addToDLQ (T.pack C.ecRedisDLQStream) deleteEntries stillFailedD "Delete"
          pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) stillFailedD
          pure (length cSucc + length uSucc + length dSucc + length recoveredD)
        else pure (length cSucc + length uSucc + length dSucc + length recoveredD)
    else pure (length cSucc + length uSucc + length dSucc)
  where
    pureRightExceptT = ExceptT . (Right <$>)

process :: Text -> Integer -> Flow Int
process dbStreamKey count = do
  _beforeProcess <- EL.getCurrentDateInMillis

  -- Report items pending in this stream shard
  streamLen <- RQ.getRedisStreamLength dbStreamKey
  case streamLen of
    Right len -> void $ publishDBSyncMetric $ Event.DrainerItemsPending (fromIntegral len)
    Left _ -> pure ()

  commands <- peekDBCommand dbStreamKey count
  case commands of
    Left err -> do
      void $ publishDBSyncMetric Event.PeekDBCommandError
      void $ publishDBSyncMetric Event.DrainerErrorsTotal
      EL.logInfo ("PEEK_DB_COMMAND_ERROR" :: Text) $ show err
      pure 0
    Right Nothing -> do
      void $ publishDBSyncMetric $ Event.DrainerBatchSize 0
      pure 0
    Right (Just c) -> do
      void $ publishDBSyncMetric $ Event.DrainerBatchSize (length c)
      res <- run c
      _afterProcess <- EL.getCurrentDateInMillis
      void $ publishProcessLatency "QueryExecutionTime" (int2Double (_afterProcess - _beforeProcess))
      void flushKafkaProducerAndPublishMetrics
      pure res
  where
    run :: [(EL.KVDBStreamEntryID, [(Text, ByteString)])] -> Flow Int
    run entries = do
      commands <- catMaybes <$> traverse (parseDBCommand dbStreamKey) entries
      let updateEntries = mapMaybe filterUpdateCommands commands
          deleteEntries = mapMaybe filterDeleteCommands commands
          createDataEntries = mapMaybe filterCreateCommands commands

      dbsyncOperationsOutput <- runExceptT $ runCriticalDBSyncOperations dbStreamKey updateEntries deleteEntries createDataEntries
      case dbsyncOperationsOutput of
        Left cnt -> do
          void $ publishDBSyncMetric Event.DrainerErrorsTotal
          stopDrainer
          pure cnt
        Right cnt -> pure cnt

startDBSync :: Flow ()
startDBSync = do
  sessionId <- EL.runIO genSessionId
  EL.setLoggerContext "session-id" (DTE.decodeUtf8 sessionId)
  readinessFlag <- EL.runIO newEmptyMVar
  void $ EL.runIO $ installHandler sigINT (Catch $ onSigINT readinessFlag) Nothing
  void $ EL.runIO $ installHandler sigTERM (Catch $ onSigTERM readinessFlag) Nothing
  threadPerPodCount <- EL.runIO Env.getThreadPerPodCount
  EL.logInfo ("Number for threads running per pod: " <> show threadPerPodCount :: Text) (show threadPerPodCount)
  eitherConfig <- getDBSyncConfig
  syncConfig <- case eitherConfig of
    Right c -> pure c
    Left _ -> {-(EL.info ("Could not read config: " <> show err)) *> -} pure Env.defaultDBSyncConfig
  EL.logInfo ("Empty retry: " :: Text) (show $ _emptyRetry syncConfig)
  EL.logInfo ("Rate limit number: " :: Text) (show $ _rateLimitN syncConfig)
  EL.logInfo ("Rate limit window: " :: Text) (show $ _rateLimitWindow syncConfig)
  EL.logInfo ("Stream read count: " :: Text) (show $ _streamReadCount syncConfig)

  dbSyncStreamEnv <- EL.runIO Env.getDBSyncStream
  let dbSyncStream =
        if dbSyncStreamEnv == ""
          then C.ecRedisDBStream
          else dbSyncStreamEnv
  stateRef <-
    EL.runIO $
      newIORef $
        StateRef
          { _config = syncConfig,
            _history = C.emptyHistory
          }
  forever $ do
    getAndSetKvConfigs

    StateRef
      { _config = config,
        _history = history
      } <-
      EL.runIO $ readIORef stateRef

    stopRequested <- EL.runIO $ isJust <$> tryTakeMVar readinessFlag
    when stopRequested $ do
      EL.logInfo ("RECEIVED SIGINT/SIGTERM" :: Text) "Graceful shutdown: draining final batch before exit"
      dbStreamKey <- getStreamName $ T.pack dbSyncStream
      case dbStreamKey of
        Nothing -> pure ()
        Just streamName -> do
          void $ try @_ @SomeException (process streamName (_streamReadCount config))
          void $ RQ.deleteKey [streamName <> "_lock"]
      void flushKafkaProducerAndPublishMetrics
      EL.runIO shutDownHandler

    isDrainingPaused <- fromMaybe False <$> getValueFromRedis C.drainerStopKey
    history' <-
      if isDrainingPaused
        then publishDBSyncMetric (Event.DrainerStopStatus 1) >> EL.runIO ((delay =<< Env.getDrainerRetryDelay) $> history)
        else do
          void $ publishDBSyncMetric $ Event.DrainerStopStatus 0
          checkBackpressure
          dbStreamKey <- getStreamName $ T.pack dbSyncStream
          case dbStreamKey of
            Nothing -> pure history
            Just streamName -> do
              EL.logDebug ("Stream name is: " <> show streamName :: Text) streamName
              history' <-
                try (process streamName (_streamReadCount config)) >>= \case
                  Left (ex :: SomeException) -> do
                    EL.logError ("DB command failed: " :: Text) (T.pack (show ex))
                    void $ publishDBSyncMetric $ Event.DrainerErrorsTotal
                    rateLimit config history 1
                  Right 0 -> EL.runIO $ delay (_emptyRetry config) $> history
                  Right n -> rateLimit config history n
              _ <- RQ.deleteKey [streamName <> "_lock"]
              pure history'
    EL.runIO $ modifyIORef stateRef (\st -> st {_history = history'})
  where
    rateLimit config history count = do
      time <- EL.getCurrentDateInMillis
      let (history', waitTime) = tryRateLimiter (_rateLimitN config) (_rateLimitWindow config) history time count
      EL.runIO $ delay =<< Env.getDrainerExecutionDelay
      void $
        if waitTime == 0
          then pure ()
          else EL.runIO $ delay waitTime
      pure history'

getAndSetKvConfigs :: Flow ()
getAndSetKvConfigs = do
  now <- EL.runIO getCurrentTime
  kvConfigLastUpdatedTime <- EL.getOption KBT.KvConfigLastUpdatedTime >>= maybe (EL.setOption KBT.KvConfigLastUpdatedTime now >> pure now) pure
  kvConfigUpdateFrequency <- EL.getOption KBT.KvConfigUpdateFrequency >>= maybe (pure 10) pure
  when (round (diffUTCTime now kvConfigLastUpdatedTime) > kvConfigUpdateFrequency) $ do
    fetchAndSetKvConfigs
    EL.setOption KBT.KvConfigLastUpdatedTime now
  pure ()

fetchAndSetKvConfigs :: Flow ()
fetchAndSetKvConfigs = do
  Env {..} <- ask
  let kvConfigsQuery = "SELECT config_value FROM " <> _esqDBCfg.connectSchemaName <> ".system_configs WHERE id = 'kv_configs'" :: T.Text
  res <- EL.runIO $ withResource _connectionPool $ \conn -> PG.query_ conn (PGS.Query $ DTE.encodeUtf8 kvConfigsQuery) :: IO [Only T.Text]
  case res of
    [Only kvConfigs] -> do
      let decodedKVConfigs = decodeFromText' @Tables (Just kvConfigs)
      case decodedKVConfigs of
        Just decodedKVConfigs' -> EL.setOption KBT.Tables decodedKVConfigs'
        Nothing -> do
          EL.logError ("KV_CONFIG_DECODE_FAILURE" :: Text) ("Failed to decode kv configs" :: Text)
          publishDBSyncMetric Event.KvConfigDecodeFailure >> stopDrainer
    err -> do
      EL.logError ("KV_CONFIG_DECODE_FAILURE" :: Text) ("Failed to fetch kv configs" <> show err :: Text)
      publishDBSyncMetric Event.KvConfigDecodeFailure >> stopDrainer
      EL.throwException (InternalError "Failed to fetch kv configs")

flushKafkaProducerAndPublishMetrics :: Flow ()
flushKafkaProducerAndPublishMetrics = do
  Env {..} <- ask
  _beforeFlush <- EL.getCurrentDateInMillis
  flushResult <- try @_ @SomeException $ EL.runIO $ KafkaProd.flushProducer _kafkaProducerTools.producer
  case flushResult of
    Left err -> do
      EL.logError ("KAFKA_FLUSH_ERROR" :: Text) (T.pack $ show err)
      stopDrainer
    Right _ -> do
      _afterFlush <- EL.getCurrentDateInMillis
      EL.logDebug ("KafkaFlushTime : " :: Text) ("Time taken to flush kafka producer in rider-drainer : " <> show (int2Double (_afterFlush - _beforeFlush)) <> "ms")
      void $ publishProcessLatency "KafkaFlushTime" (int2Double (_afterFlush - _beforeFlush))
