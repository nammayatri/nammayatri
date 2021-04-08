module Beckn.Utils.Flow (insertLogContext, lookupLogContext) where

import qualified Data.HashMap.Strict as HM
import EulerHS.Prelude
import EulerHS.Runtime

insertLogContext :: Text -> Text -> FlowRuntime -> FlowRuntime
insertLogContext key val currRt = newRt
  where
    oldLoggerRt = _loggerRuntime $ _coreRuntime currRt
    newLoggerRt = insertLogContext' key val oldLoggerRt
    newRt = currRt {_coreRuntime = CoreRuntime newLoggerRt}

insertLogContext' :: Text -> Text -> LoggerRuntime -> LoggerRuntime
insertLogContext' key val (MemoryLoggerRuntime ff _oldLc lvl msg c) =
  let newLcHM = HM.insert key val _oldLc
   in MemoryLoggerRuntime ff newLcHM lvl msg c
insertLogContext' key val LoggerRuntime {..} =
  let newLcHM = HM.insert key val _logContext
   in LoggerRuntime _flowFormatter newLcHM _logLevel _logRawSql _logCounter _logMaskingConfig _logLoggerHandle

lookupLogContext :: Text -> FlowRuntime -> Maybe Text
lookupLogContext key currRt = lookupLogContext' key loggerRt
  where
    loggerRt = _loggerRuntime $ _coreRuntime currRt

lookupLogContext' :: Text -> LoggerRuntime -> Maybe Text
lookupLogContext' key (MemoryLoggerRuntime _ _lc _ _ _) =
  HM.lookup key _lc
lookupLogContext' key LoggerRuntime {..} =
  HM.lookup key _logContext
