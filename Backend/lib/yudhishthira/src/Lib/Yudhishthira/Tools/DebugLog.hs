module Lib.Yudhishthira.Tools.DebugLog
  ( runLogicsWithDebugLog,
    computeNammaTagsWithDebugLog,
    computeNammaTagsWithExpiryAndDebugLog,
    setJsonLogicDebugFlags,
    checkDebugLogFlags,
    SetJsonLogicDebugReq (..),
    DebugLogIdentifier (..),
    CallerApp (..),
    debugLogIdentifierToText,
  )
where

import qualified Data.Aeson as A
import qualified Data.String.Conversions as CS
import qualified Data.Text as T
import qualified Data.Time.Format as Time
import qualified Data.Time.Format.ISO8601 as ISO
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CHConfig
import qualified Kernel.Storage.Clickhouse.Queries as CHQueries
import Kernel.Storage.Clickhouse.Types (ClickhouseExpr (..))
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.HideSecrets (HideSecrets (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Event as Event
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Tools.Utils (mkTagNameValue, mkTagNameValueExpiry, runLogics)
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.Application as YA

-- | Validated identifier: either a dynamic-logic domain or a NammaTag application event.
data DebugLogIdentifier
  = LogicDomainId LYT.LogicDomain
  | NammaTagEventId YA.ApplicationEvent
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Convert a typed identifier to the Text key used in Redis.
debugLogIdentifierToText :: DebugLogIdentifier -> Text
debugLogIdentifierToText (LogicDomainId domain) = T.pack $ show domain
debugLogIdentifierToText (NammaTagEventId event) = T.pack $ show event

-- | Identifies which app triggered the debug log
data CallerApp = Rider | Driver
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

callerAppToText :: CallerApp -> Text
callerAppToText Rider = "rider"
callerAppToText Driver = "driver"

-- | Request to enable/disable debug logging for a domain/event
data SetJsonLogicDebugReq = SetJsonLogicDebugReq
  { identifier :: DebugLogIdentifier, -- either a LogicDomain or an ApplicationEvent
    enabled :: Bool,
    startTime :: UTCTime,
    endTime :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance HideSecrets SetJsonLogicDebugReq where
  hideSecrets = identity

-- Redis key helpers
mkDebugEnabledKey :: Id LYT.MerchantOperatingCity -> Text -> Text
mkDebugEnabledKey mocId identifier' = "JSONLogicDebug:" <> mocId.getId <> ":" <> identifier' <> ":enabled"

mkDebugStartTimeKey :: Id LYT.MerchantOperatingCity -> Text -> Text
mkDebugStartTimeKey mocId identifier' = "JSONLogicDebug:" <> mocId.getId <> ":" <> identifier' <> ":startTime"

mkDebugEndTimeKey :: Id LYT.MerchantOperatingCity -> Text -> Text
mkDebugEndTimeKey mocId identifier' = "JSONLogicDebug:" <> mocId.getId <> ":" <> identifier' <> ":endTime"

-- | Dashboard API handler: set/unset debug logging flags in Redis
setJsonLogicDebugFlags ::
  (CacheFlow m r) =>
  Id LYT.MerchantOperatingCity ->
  SetJsonLogicDebugReq ->
  m ()
setJsonLogicDebugFlags mocId req = do
  let identifierKey = debugLogIdentifierToText req.identifier
  now <- getCurrentTime
  let ttlSeconds = max 1 $ ceiling $ diffUTCTime req.endTime now + 3600 -- extra hour buffer
  if req.enabled
    then do
      Hedis.setExp (mkDebugEnabledKey mocId identifierKey) ("1" :: Text) ttlSeconds
      Hedis.setExp (mkDebugStartTimeKey mocId identifierKey) (T.pack $ ISO.iso8601Show req.startTime) ttlSeconds
      Hedis.setExp (mkDebugEndTimeKey mocId identifierKey) (T.pack $ ISO.iso8601Show req.endTime) ttlSeconds
    else do
      Hedis.del (mkDebugEnabledKey mocId identifierKey)
      Hedis.del (mkDebugStartTimeKey mocId identifierKey)
      Hedis.del (mkDebugEndTimeKey mocId identifierKey)

-- | Check if debug logging is enabled for an identifier and current time is within window
checkDebugLogFlags ::
  (CacheFlow m r) =>
  Id LYT.MerchantOperatingCity ->
  Text ->
  m Bool
checkDebugLogFlags mocId identifier' = do
  mbEnabled :: Maybe Text <- Hedis.get (mkDebugEnabledKey mocId identifier')
  case mbEnabled of
    Just "1" -> do
      mbStartStr :: Maybe Text <- Hedis.get (mkDebugStartTimeKey mocId identifier')
      mbEndStr :: Maybe Text <- Hedis.get (mkDebugEndTimeKey mocId identifier')
      now <- getCurrentTime
      let mbInWindow = do
            startStr <- mbStartStr
            endStr <- mbEndStr
            startT <- ISO.iso8601ParseM (T.unpack startStr)
            endT <- ISO.iso8601ParseM (T.unpack endStr)
            pure $ now >= startT && now <= endT
      pure $ fromMaybe False mbInWindow
    _ -> pure False

-- | Insert a debug log row into ClickHouse json_logic_transactions table
insertJsonLogicTransaction ::
  (MonadFlow m, CHConfig.ClickhouseFlow m r, Log m) =>
  CallerApp -> -- caller app (rider/driver)
  Text -> -- domain/event identifier
  A.Value -> -- inputData
  A.Value -> -- logic
  A.Value -> -- outputData
  m ()
insertJsonLogicTransaction callerApp domain inputData logic outputData = do
  transactionId <- generateGUID
  now <- getCurrentTime
  let escapeStr = T.replace "'" "\\'" . T.replace "\\" "\\\\"
  let inputStr = escapeStr $ CS.cs $ A.encode inputData
  let logicStr = escapeStr $ CS.cs $ A.encode logic
  let outputStr = escapeStr $ CS.cs $ A.encode outputData
  let timestampStr = T.pack $ formatDateTimeUTC now
  let callerAppStr = callerAppToText callerApp
  let query =
        "INSERT INTO app_monitor.json_logic_transactions (transactionId, domain, timestamp, inputData, logic, outputData, caller_app) VALUES ('"
          <> T.unpack transactionId
          <> "', '"
          <> T.unpack domain
          <> "', '"
          <> T.unpack timestampStr
          <> "', '"
          <> T.unpack inputStr
          <> "', '"
          <> T.unpack logicStr
          <> "', '"
          <> T.unpack outputStr
          <> "', '"
          <> T.unpack callerAppStr
          <> "')"
  eResult <- CHQueries.runExecQuery' (ExprStr query) CHConfig.APP_SERVICE_CLICKHOUSE
  case eResult of
    Right _ -> logDebug $ "Debug log inserted for domain: " <> domain
    Left err -> logWarning $ "Debug log ClickHouse insert failed: " <> T.pack err

formatDateTimeUTC :: UTCTime -> String
formatDateTimeUTC = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Wrapper around runLogics that adds debug logging to ClickHouse
runLogicsWithDebugLog ::
  ( MonadFlow m,
    ToJSON a,
    CHConfig.ClickhouseFlow m r,
    CacheFlow m r
  ) =>
  CallerApp ->
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  [A.Value] ->
  a ->
  m LYT.RunLogicResp
runLogicsWithDebugLog callerApp mocId domain logics data_ = do
  resp <- runLogics logics data_
  let domainStr = T.pack $ show domain
  fork "jsonLogicDebugLog" $ do
    shouldLog <- checkDebugLogFlags mocId domainStr
    when shouldLog $ do
      let inputVal = A.toJSON data_
      let logicVal = A.toJSON logics
      let outputVal = resp.result
      handle (\(e :: SomeException) -> logWarning $ "Debug log to ClickHouse failed: " <> show e) $
        insertJsonLogicTransaction callerApp domainStr inputVal logicVal outputVal
  return resp

-- | Wrapper around computeNammaTags with debug logging
computeNammaTagsWithDebugLog ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema,
    ToJSON a,
    CHConfig.ClickhouseFlow m r
  ) =>
  CallerApp ->
  Id LYT.MerchantOperatingCity ->
  YA.ApplicationEvent ->
  a ->
  m [LYT.TagNameValue]
computeNammaTagsWithDebugLog callerApp merchantOpCityId event sourceData_ = do
  let sourceData = A.toJSON sourceData_
  let req = LYT.YudhishthiraDecideReq {merchantOperatingCityId = merchantOpCityId, source = LYT.Application event, sourceData}
  resp <- Event.yudhishthiraDecide req
  let tags = resp.tags <&> (\tag -> mkTagNameValue (LYT.TagName tag.tagName) tag.tagValue)
  -- Debug log in forked thread
  let eventStr = T.pack $ show event
  fork "nammaTagDebugLog" $ do
    shouldLog <- checkDebugLogFlags merchantOpCityId eventStr
    when shouldLog $ do
      let inputVal = sourceData
      let logicVal = A.String "NammaTag"
      let outputVal = A.toJSON (resp.tags <&> (\t -> A.object ["tagName" A..= t.tagName, "tagValue" A..= (T.pack $ show t.tagValue)]))
      handle (\(e :: SomeException) -> logWarning $ "NammaTag debug log to ClickHouse failed: " <> show e) $
        insertJsonLogicTransaction callerApp eventStr inputVal logicVal outputVal
  pure tags

-- | Wrapper around computeNammaTagsWithExpiry with debug logging
computeNammaTagsWithExpiryAndDebugLog ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasYudhishthiraTablesSchema,
    ToJSON a,
    CHConfig.ClickhouseFlow m r
  ) =>
  CallerApp ->
  Id LYT.MerchantOperatingCity ->
  YA.ApplicationEvent ->
  a ->
  m [LYT.TagNameValueExpiry]
computeNammaTagsWithExpiryAndDebugLog callerApp merchantOpCityId event sourceData_ = do
  let sourceData = A.toJSON sourceData_
  let req = LYT.YudhishthiraDecideReq {merchantOperatingCityId = merchantOpCityId, source = LYT.Application event, sourceData}
  resp <- Event.yudhishthiraDecide req
  now <- getCurrentTime
  let tags = resp.tags <&> (\tag -> mkTagNameValueExpiry (LYT.TagName tag.tagName) tag.tagValue tag.tagValidity now)
  -- Debug log in forked thread
  let eventStr = T.pack $ show event
  fork "nammaTagExpiryDebugLog" $ do
    shouldLog <- checkDebugLogFlags merchantOpCityId eventStr
    when shouldLog $ do
      let inputVal = sourceData
      let logicVal = A.String "NammaTag"
      let outputVal = A.toJSON (resp.tags <&> (\t -> A.object ["tagName" A..= t.tagName, "tagValue" A..= (T.pack $ show t.tagValue)]))
      handle (\(e :: SomeException) -> logWarning $ "NammaTag debug log to ClickHouse failed: " <> show e) $
        insertJsonLogicTransaction callerApp eventStr inputVal logicVal outputVal
  pure tags
