module DBSync.Create where

import Euler.Config.Config as Config
import Euler.Config.Env
import Euler.Types.DBSync
import Euler.Types.Event as Event
import Euler.Utils.Logging
import Euler.Utils.Utils
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.Language as EL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types as ET
import qualified Kernel.Beam.Types as KBT
import Storage.Beam.RegistrationToken as BeamRT
import System.CPUTime

runCreateCommands :: Show b => [(CreateDBCommand, b)] -> ReaderT Env EL.Flow [Either [KVDBStreamEntryID] [KVDBStreamEntryID]]
runCreateCommands cmds = do
  --pgSQLDBConf <- Config.getEulerPgDbConf
  dbConf <- L.getOption KBT.PsqlDbCfg
  runCreate dbConf ("RegistrationTokenT" :: Text) [(obj, val, entryId) | (CreateDBCommand entryId _ _ _ _ (BeamRT.RegistrationTokenT obj), val) <- cmds]
  where
    runCreate dbConf model object = do
      let dbObjects = map (\(dbObject, _, _) -> dbObject) object
          byteStream = map (\(_, bts, _) -> bts) object
          entryIds = map (\(_, _, entryId) -> entryId) object
          cmdsToErrorQueue = map ("command" :: String,) byteStream
      maxRetries <- EL.runIO getMaxRetries
      if null object then pure [Right []] else runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds 0 maxRetries False

    runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries ignoreDuplicates = do
      t1 <- EL.getCurrentDateInMillis
      cpuT1 <- EL.runIO getCPUTime
      res <- CDB.createMultiSqlWoReturning dbConf dbObjects ignoreDuplicates
      t2 <- EL.getCurrentDateInMillis
      cpuT2 <- EL.runIO getCPUTime
      case (res, index) of -- Ignore duplicate entry
        (Right _, _) -> do
          EL.logInfoV ("Drainer Info" :: Text) $ createDBLogEntry model "CREATE" (t2 - t1) (cpuT2 - cpuT1) dbObjects -- Logging group latencies
          pure [Right entryIds]
        (Left (ET.DBError (ET.SQLError (ET.MysqlError (ET.MysqlSqlError 1062 err))) _), _) -> do
          EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> model <> ", Error message: " <> err)
          void $ publishDBSyncMetric $ Event.DuplicateEntryCreate model
          -- Is retry delay needed here? :/
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True -- Should retry count be increased here? :/
        (Left (ET.DBError (ET.SQLError (ET.PostgresError (ET.PostgresSqlError ("23505" :: Text) _ errMsg _ _))) _), _) -> do
          EL.logInfo ("DUPLICATE_ENTRY" :: Text) ("Got duplicate entry for model: " <> model <> ", Error message: " <> errMsg)
          void $ publishDBSyncMetric $ Event.DuplicateEntryCreate model
          -- Is retry delay needed here? :/
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds index maxRetries True -- Should retry count be increased here? :/
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" model
          EL.runIO $ delay =<< getRetryDelay
          runCreateWithRecursion dbConf model dbObjects cmdsToErrorQueue entryIds (index + 1) maxRetries ignoreDuplicates -- Should we pass the same ignoreDuplicates or should we pass False here.
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Create" model
          EL.logError ("Create failed: " :: Text) (show cmdsToErrorQueue <> "\n Error: " <> show x :: Text)
          pure [Left entryIds]
