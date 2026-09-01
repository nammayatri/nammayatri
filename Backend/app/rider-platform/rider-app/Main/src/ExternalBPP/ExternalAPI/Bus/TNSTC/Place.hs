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

-- | TNSTC prices and books on a 3-char place *code* (PUU / CHE), while our stations are keyed
-- by the numeric stop code. The mapping lives on the station as `providerCode` and is static,
-- so it is cached for an hour and falls back to the caller's value when the feed has no code.
--
-- Lives here rather than in the select handler so both select and confirm can call it --
-- Order.hs cannot import Domain.Action.UI.FRFSTicketService without an import cycle, which is
-- the only reason the resolved codes were previously persisted on the quote.
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
        Right stop | not (T.null (T.strip stop.providerCode)) -> Just (T.strip stop.providerCode)
        _ -> Nothing
  return $ fromMaybe fallbackCode mbCode
