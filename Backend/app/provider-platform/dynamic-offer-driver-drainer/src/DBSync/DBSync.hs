module DBSync.DBSync where

import qualified Config.Env as Env
import qualified Constants as C
import Control.Monad.Trans.Except
import DBSync.Create
import DBSync.Delete
import DBSync.Update
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T hiding (elem)
import qualified Data.Text.Encoding as DTE
import qualified Data.Vector as V
import qualified Database.Redis as R
import EulerHS.Language (runIO)
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (fail, id, succ)
import qualified EulerHS.Types as ET
import GHC.Float (int2Double)
import Kafka.Producer as KafkaProd
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Types.Config
import Types.DBSync
import qualified Types.Event as Event
import Utils.Config
import Utils.Parse
import Utils.Redis
import qualified Utils.Redis as RQ
import Utils.Utils

peekDBCommand :: Text -> Integer -> Flow (Either ET.KVDBReply (Maybe [(EL.KVDBStreamEntryID, [(Text, ByteString)])]))
peekDBCommand dbStreamKey count = do
  {- Either Error (Maybe (Array Entry)) -}
  dbReadResponse <-
    ((find (\x -> decodeToText (R.stream x) == dbStreamKey) =<<) <$>)
      <$> RQ.readValueFromRedisStream dbStreamKey (EL.EntryID $ EL.KVDBStreamEntryID 0 0) count
  pure $ parseReadStreams <$> dbReadResponse
  where
    parseReadStreams = (>>= (\(R.XReadResponse _ entries) -> Just $ entryToTuple <$> entries))
    entryToTuple (R.StreamsRecord recordId items) = (parseStreamEntryId recordId, first decodeToText <$> items)

-- Try to Parse to DBCommand
-- If the key is dirty which means its already been pushed to mysql, then we will discard other we return the parsed DBCommand
parseDBCommand :: Text -> (EL.KVDBStreamEntryID, [(Text, ByteString)]) -> Flow (Maybe (EL.KVDBStreamEntryID, DBCommand, ByteString))
parseDBCommand dbStreamKey entries =
  case entries of
    (id, [("command", val)]) ->
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
              mbModel = case AKM.lookup "contents" o of
                Just _commandArray@(A.Array a) -> case V.last a of
                  _commandObject@(A.Object command) -> case AKM.lookup "tag" command of
                    Just (A.String modelTag) -> return modelTag
                    _ -> Nothing
                  _ -> Nothing
                _ -> Nothing
           in (mbAction, mbModel)
        _ -> (Nothing, Nothing)

-- Add Retry Logic For Redis Drop
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
  isForcePushEnabled <- pureRightExceptT $ fromMaybe False <$> getValueFromRedis C.forceDrainEnabledKey
  {- run bulk-inserts parallel -}
  (cSucc, cFail) <- pureRightExceptT $ executeInSequence runCreate ([], []) dbStreamKey createDataEntries
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "Create" (fromIntegral $ length cSucc)
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "CreateInBatch" (if null cSucc then 0 else 1)
  void $
    if null cSucc
      then pureRightExceptT $ publishDBSyncMetric $ Event.QueryDrainLatency "Create" 0
      else pureRightExceptT $ traverse_ (publishDrainLatency "Create") cSucc
  {- drop successful inserts from stream -}
  void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) cSucc
  {- fail if any insert fails -}
  void $
    if not (null cFail)
      then do
        if isForcePushEnabled
          then do
            EL.logError ("CREATE FAILED: Force Sync is enabled" :: Text) (show cFail :: Text)
            void $ pureRightExceptT $ addValueToErrorQueue (T.pack C.ecRedisFailedStream) ((\(_, bts) -> ("command", bts)) <$> filter (\(id, _) -> id `elem` cFail) createDataEntries)
            pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) cFail
            pure (length cSucc)
          else do
            EL.logError ("CREATE FAILED: Force Sync is not enabled, so stopping the drainer" :: Text) (show cFail :: Text)
            throwE (length cSucc)
      else pure (length cSucc)
  {- run updates parallel -}
  (uSucc, uFail) <- pureRightExceptT $ executeInSequence runUpdate ([], []) dbStreamKey updateEntries
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "Update" (fromIntegral $ length uSucc)
  void $
    if null uSucc
      then pureRightExceptT $ publishDBSyncMetric $ Event.QueryDrainLatency "Update" 0
      else pureRightExceptT $ traverse_ (publishDrainLatency "Update") uSucc
  {- drop successful updates from stream -}
  void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) uSucc
  {- fail if any update fails -}
  void $
    if not (null uFail)
      then do
        if isForcePushEnabled
          then do
            EL.logError ("UPDATE FAILED: Force Sync is enabled" :: Text) (show uFail :: Text)
            -- void $ pureRightExceptT $ addValueToErrorQueue (T.pack C.ecRedisFailedStream) ((\(_, bts) -> ("command", bts)) <$> filter (\(UpdateDBCommand id _ _ _ _ _, _) -> id `elem` uFail) updateEntries)
            void $ pureRightExceptT $ addValueToErrorQueue (T.pack C.ecRedisFailedStream) ((\(_, bts) -> ("command", bts)) <$> filter (\(id, _) -> id `elem` uFail) updateEntries)
            pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) uFail
            pure (length cSucc + length uSucc)
          else do
            EL.logError ("UPDATE FAILED: Force Sync is not enabled, so stopping the drainer" :: Text) (show uFail :: Text)
            throwE (length cSucc + length uSucc)
      else pure (length cSucc + length uSucc)
  {- run deletes parallel -}
  (dSucc, dFail) <- pureRightExceptT $ executeInSequence runDelete ([], []) dbStreamKey deleteEntries
  void $ pureRightExceptT $ publishDBSyncMetric $ Event.DrainerQueryExecutes "Delete" (fromIntegral $ length dSucc)
  void $
    if null dSucc
      then pureRightExceptT $ publishDBSyncMetric $ Event.QueryDrainLatency "Delete" 0
      else pureRightExceptT $ traverse_ (publishDrainLatency "Delete") dSucc
  {- drop successful deletes from stream -}
  void $ pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) dSucc
  {- fail if any delete fails -}
  if not (null dFail)
    then do
      if isForcePushEnabled
        then do
          EL.logError ("DELETE FAILED: Force Sync is enabled" :: Text) (show dFail :: Text)
          void $ pureRightExceptT $ addValueToErrorQueue (T.pack C.ecRedisFailedStream) ((\(_, bts) -> ("command", bts)) <$> filter (\(id, _) -> id `elem` dFail) deleteEntries)
          pureRightExceptT $ traverse_ (dropDBCommand dbStreamKey) dFail
          pure (length cSucc + length uSucc + length dSucc)
        else do
          EL.logError ("DELETE FAILED: Force Sync is not enabled, so stopping the drainer" :: Text) (show dFail :: Text)
          throwE (length cSucc + length uSucc + length dSucc)
    else pure (length cSucc + length uSucc + length dSucc)
  where
    pureRightExceptT = ExceptT . (Right <$>)

process :: Text -> Integer -> Flow Int
process dbStreamKey count = do
  {- TODO: Need To write CPU Latencies -}
  _beforeProcess <- EL.getCurrentDateInMillis
  {- read command if available from stream -}
  commands <- peekDBCommand dbStreamKey count
  case commands of
    Left err -> do
      void $ publishDBSyncMetric Event.PeekDBCommandError
      EL.logInfo ("PEEK_DB_COMMAND_ERROR" :: Text) $ show err
      pure 0
    Right Nothing -> do
      pure 0
    Right (Just c) -> do
      res <- run c
      _afterProcess <- EL.getCurrentDateInMillis
      void $ publishProcessLatency "QueryExecutionTime" (int2Double (_afterProcess - _beforeProcess))
      Env {..} <- ask
      _beforeFlush <- EL.getCurrentDateInMillis
      EL.runIO $ KafkaProd.flushProducer _kafkaConnection
      _afterFlush <- EL.getCurrentDateInMillis
      void $ publishProcessLatency "KafkaFlushTime" (int2Double (_afterFlush - _beforeFlush))
      pure res
  where
    {- Let's try to decode and run the commands -}
    {- time taken to process batch -}

    run :: [(EL.KVDBStreamEntryID, [(Text, ByteString)])] -> Flow Int
    run entries = do
      commands <- catMaybes <$> traverse (parseDBCommand dbStreamKey) entries
      let updateEntries = mapMaybe filterUpdateCommands commands
          deleteEntries = mapMaybe filterDeleteCommands commands
          createDataEntries = mapMaybe filterCreateCommands commands

      dbsyncOperationsOutput <- runExceptT $ runCriticalDBSyncOperations dbStreamKey updateEntries deleteEntries createDataEntries
      case dbsyncOperationsOutput of
        Left cnt -> do
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
  threadPerPodCount <- runIO Env.getThreadPerPodCount
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
    stopRequested <- EL.runIO $ isJust <$> tryTakeMVar readinessFlag
    EL.runIO $ when stopRequested shutDownHandler

    StateRef
      { _config = config,
        _history = history
      } <-
      EL.runIO $ readIORef stateRef

    isDrainingPaused <- fromMaybe False <$> getValueFromRedis C.drainerStopKey
    history' <-
      if isDrainingPaused
        then publishDBSyncMetric (Event.DrainerStopStatus 1) >> EL.runIO ((delay =<< Env.getDrainerRetryDelay) $> history)
        else do
          void $ publishDBSyncMetric $ Event.DrainerStopStatus 0
          dbStreamKey <- getStreamName $ T.pack dbSyncStream
          case dbStreamKey of
            Nothing -> pure history
            Just streamName -> do
              EL.logDebug ("Stream name is: " <> show streamName :: Text) streamName
              history' <-
                try (process streamName (_streamReadCount config)) >>= \case
                  Left (ex :: SomeException) -> do
                    EL.logError ("DB command failed: " :: Text) (T.pack (show ex))
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
