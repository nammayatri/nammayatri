module SharedLogic.InboundGate (shouldDropSigned) where

import qualified Domain.Types as Domain
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Types.Beckn.Domain as BecknDomain
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.WhiteListOrg as CQWLO

shouldDropSigned ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, Metrics.CoreMetrics m) =>
  Text ->
  Id DM.Merchant ->
  Maybe Text ->
  m Bool
shouldDropSigned _ _ Nothing = pure False
shouldDropSigned action mId (Just peerSubId) = do
  mbEntry <- CQWLO.findBySubscriberIdAndDomainAndMerchantId (ShortId peerSubId) BecknDomain.MOBILITY mId
  let dropIt = ((mbEntry >>= (.supportedBecknProtocols)) >>= listToMaybe) == Just Domain.Beckn_V3
  when dropIt $ Metrics.incrementGenericMetrics $ "beckn_signed_dropped_" <> action
  pure dropIt
