module Storage.CachedQueries.OTPRest.Common where

import Domain.Types.IntegratedBPPConfig
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.Flow as Flow
import SharedLogic.IntegratedBPPConfig (getGimsBaseUrl)

getChildrenStationsCodes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => IntegratedBPPConfig -> Text -> m [Text]
getChildrenStationsCodes integratedBPPConfig stopCode = do
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  Flow.getStopChildren baseUrl (integratedBPPConfig.feedKey) stopCode
