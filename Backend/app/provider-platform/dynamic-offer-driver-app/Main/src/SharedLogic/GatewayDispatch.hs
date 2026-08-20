{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.GatewayDispatch
  ( dispatchAction,
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
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId) -- TODO: Handle later
  case (listToMaybe merchant.gatewayAndRegistryPriorityList, mbPeerSubId) of
    (Just Domain.Fabric, Just peerSubId) -> fabricPath merchant peerSubId
    _ -> signedCall
  where
    fabricPath merchant peerSubId = do
      protocols <- lookupPeerProtocols peerSubId merchant.id
      case protocols of
        [] -> signedCall
        (primary : rest) -> do
          forM_ rest $ \case
            Domain.Beckn_V3 -> void $ fork "shadow-fire-V3" (void $ fireFabric merchant)
            Domain.Beckn_V2 -> pure ()
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
          subId = merchant.subscriberId.getShortId
          url = base {baseUrlPath = baseUrlPath base <> "/bpp/caller/" <> T.unpack subId}
          bppReceiverUri = showBaseUrl $ base {baseUrlPath = baseUrlPath base <> "/bpp/receiver/" <> T.unpack subId}
          mappedAction = BecknContext.fabricActionName action
          jsonReq = A.toJSON req
          mutated = case mbNetworkId of
            Just nid -> BecknContext.mutateFabricContextBpp nid bppReceiverUri jsonReq
            Nothing -> jsonReq
      Metrics.incrementGenericMetrics $ "beckn_outbound_bpp_" <> mappedAction
      unsignedCall url mappedAction mutated
