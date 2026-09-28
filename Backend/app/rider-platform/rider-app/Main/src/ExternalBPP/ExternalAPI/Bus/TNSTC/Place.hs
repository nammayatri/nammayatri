module ExternalBPP.ExternalAPI.Bus.TNSTC.Place (tnstcPlaceCode) where

import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Utils.Common
import qualified SharedLogic.External.Nandi.Flow as NandiFlow
import qualified Tools.MultiModal as MM

tnstcPlaceCode ::
  forall m r c.
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasShortDurationRetryCfg r c,
    HasField "requestId" r (Maybe Text)
  ) =>
  DIBC.IntegratedBPPConfig ->
  Text ->
  Text ->
  m Text
tnstcPlaceCode integratedBPPConfig fallbackCode stopCode = do
  mbCode <-
    IM.withInMemCache ["tnstcPlaceCode", integratedBPPConfig.id.getId, stopCode] 3600 $ do
      baseUrl <- MM.getOTPRestServiceReq integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId
      res <- try @_ @SomeException $ NandiFlow.getStationsByGtfsIdAndStopCode baseUrl integratedBPPConfig.feedKey stopCode
      return $ case res of
        Right stop -> case stop.placeCode of
          Just d | not (T.null (T.strip d)) -> Just (T.strip d)
          _ -> Nothing
        _ -> Nothing
  return $ fromMaybe fallbackCode mbCode
