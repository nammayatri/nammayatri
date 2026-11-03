module Lib.BmsBackend.Flow where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Lib.BmsBackend.API.Events as EventsAPI
import Lib.BmsBackend.Types
import TransactionLogs.ONDC.Flow ()

-- orphan ToJSON NoContent, required by callAPI's response logging

-- | Throws on any bms failure so the caller can refuse the driver's report rather than
-- recording one bms never received.
reportEvent ::
  ( CoreMetrics m,
    MonadFlow m,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    MonadReader r m
  ) =>
  BmsBackendConfig ->
  ReportEventReq ->
  m ()
reportEvent cfg req = do
  let url = cfg.url
  void $
    withShortRetry $
      callAPI url (EventsAPI.reportEvent req) "bmsReportEvent" EventsAPI.reportEventAPI
        >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_BMS_REPORT_EVENT_API") url)
