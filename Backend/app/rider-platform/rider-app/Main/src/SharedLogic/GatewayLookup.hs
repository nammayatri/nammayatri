{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.GatewayLookup
  ( dispatchToGateway,
    dispatchAction,
  )
where

import qualified BecknV2.OnDemand.Utils.Context as BecknContext
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types as Domain
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import qualified Kernel.Types.Beckn.Domain as BecknDomain
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.WhiteListOrg as CQWLO
import qualified Storage.Queries.BecknConfig as QBC

dispatchToGateway ::
  ( MonadFlow m,
    MonadCatch m,
    Metrics.CoreMetrics m,
    ToJSON req,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv
      m
      r
      '[ "ondcGatewayUrl" ::: BaseUrl,
         "nyGatewayUrl" ::: BaseUrl,
         "fabricGatewayBaseUrl" ::: BaseUrl
       ]
  ) =>
  Id.Id DM.Merchant ->
  BecknDomain.Domain ->
  Text ->
  req ->
  (BaseUrl -> req -> m ()) ->
  (BaseUrl -> Text -> A.Value -> m ()) ->
  m ()
dispatchToGateway merchantId domain action req signedCall unsignedCall = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case listToMaybe merchant.gatewayAndRegistryPriorityList of
    Just Domain.Fabric -> fireFabric merchant
    Just Domain.NY -> asks (.nyGatewayUrl) >>= flip signedCall req
    _ -> asks (.ondcGatewayUrl) >>= flip signedCall req
  where
    fireFabric merchant = do
      base <- asks (.fabricGatewayBaseUrl)
      mbCfg <- QBC.findByMerchantIdDomainAndBecknProtocol (Just merchant.id) (show domain) (Just Domain.Beckn_V3)
      let mbNetworkId = mbCfg >>= (.networkId)
          url = base {baseUrlPath = baseUrlPath base <> "/bap/caller/" <> T.unpack merchant.bapId}
          bapReceiverUri = showBaseUrl $ base {baseUrlPath = baseUrlPath base <> "/bap/receiver/" <> T.unpack merchant.bapId}
          mappedAction = BecknContext.fabricActionName action
          jsonReq = A.toJSON req
          mutated = case mbNetworkId of
            Just nid -> BecknContext.mutateFabricContext nid bapReceiverUri jsonReq
            Nothing -> jsonReq
      Metrics.incrementGenericMetrics $ "beckn_outbound_" <> mappedAction
      unsignedCall url mappedAction mutated

dispatchAction ::
  ( MonadFlow m,
    MonadCatch m,
    Metrics.CoreMetrics m,
    ToJSON req,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl]
  ) =>
  Id.Id DM.Merchant ->
  BecknDomain.Domain ->
  Text ->
  Maybe Text ->
  req ->
  m AckResponse ->
  (BaseUrl -> Text -> A.Value -> m AckResponse) ->
  m AckResponse
dispatchAction merchantId domain action mbPeerSubId req signedCall unsignedCall = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case (listToMaybe merchant.gatewayAndRegistryPriorityList, mbPeerSubId) of
    (Just Domain.Fabric, Just peerSubId) -> fabricPath merchant peerSubId
    _ -> signedCall
  where
    fabricPath merchant peerSubId = do
      protocols <- lookupPeerProtocols peerSubId merchant.id
      case nub protocols of
        [] -> signedCall
        (primary : rest) -> do
          forM_ rest $ \p ->
            void $ fork ("shadow-fire-" <> show p) (void $ dispatchOne merchant p)
          dispatchOne merchant primary

    dispatchOne merchant Domain.Beckn_V3 = fireFabric merchant
    dispatchOne _ Domain.Beckn_V2 = signedCall

    lookupPeerProtocols peerSubId mId = do
      mbEntry <- CQWLO.findBySubscriberIdAndDomainAndMerchantId (Id.ShortId peerSubId) domain mId
      pure $ fromMaybe [] (mbEntry >>= (.supportedBecknProtocols))

    fireFabric merchant = do
      base <- asks (.fabricGatewayBaseUrl)
      mbCfg <- QBC.findByMerchantIdDomainAndBecknProtocol (Just merchant.id) (show domain) (Just Domain.Beckn_V3)
      let mbNetworkId = mbCfg >>= (.networkId)
          url = base {baseUrlPath = baseUrlPath base <> "/bap/caller/" <> T.unpack merchant.bapId}
          bapReceiverUri = showBaseUrl $ base {baseUrlPath = baseUrlPath base <> "/bap/receiver/" <> T.unpack merchant.bapId}
          mappedAction = BecknContext.fabricActionName action
          jsonReq = A.toJSON req
          mutated = case mbNetworkId of
            Just nid -> BecknContext.mutateFabricContext nid bapReceiverUri jsonReq
            Nothing -> jsonReq
      Metrics.incrementGenericMetrics $ "beckn_outbound_" <> mappedAction
      unsignedCall url mappedAction mutated
