module Beckn.Utils.Flow (addLogContext) where


import qualified Data.HashMap.Strict as HM
import EulerHS.Prelude
import EulerHS.Runtime


addLogContext :: Text -> FlowRuntime -> FlowRuntime
addLogContext lc currRt = newRt
  where
    oldLoggerRt = _loggerRuntime $ _coreRuntime currRt
    newLoggerRt = insertLogContext "log_context" lc oldLoggerRt
    newRt = currRt {_coreRuntime = CoreRuntime newLoggerRt}

insertLogContext :: Text -> Text -> LoggerRuntime -> LoggerRuntime
insertLogContext key lc (MemoryLoggerRuntime ff _oldLc lvl msg c) =
  let oldLc = HM.lookupDefault "" key _oldLc
      newLcHM = HM.insert key (oldLc <> "[" <> lc <> "]") _oldLc
   in MemoryLoggerRuntime ff newLcHM lvl msg c
insertLogContext key lc LoggerRuntime {..} =
  let oldLc = HM.lookupDefault "" key _logContext
      newLcHM = HM.insert key (oldLc <> "[" <> lc <> "]") _logContext
   in LoggerRuntime _flowFormatter newLcHM _logLevel _logRawSql _logCounter _logMaskingConfig _logLoggerHandle
