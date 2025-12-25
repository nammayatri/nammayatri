module Storage.CachedQueries.OTPRest.Common where

import Domain.Types.IntegratedBPPConfig
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as Flow
import Tools.MultiModal as MM

getChildrenStationsCodes :: (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) => IntegratedBPPConfig -> Text -> m [Text]
getChildrenStationsCodes integratedBPPConfig stopCode = do
  baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
  Flow.getStopChildren baseUrl integratedBPPConfig.feedKey stopCode
