{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.GatewayLookup
  ( resolveGatewayUrl,
    dispatchToGateways,
  )
where

import qualified BecknV2.OnDemand.Utils.Context as BecknContext
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types as Domain
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common

resolveGatewayUrl ::
  ( HasFlowEnv
      m
      r
      '[ "ondcGatewayUrl" ::: BaseUrl,
         "nyGatewayUrl" ::: BaseUrl,
         "fabricGatewayBaseUrl" ::: BaseUrl
       ]
  ) =>
  DM.Merchant ->
  Domain.GatewayAndRegistryService ->
  m BaseUrl
resolveGatewayUrl merchant = \case
  Domain.ONDC -> asks (.ondcGatewayUrl)
  Domain.NY -> asks (.nyGatewayUrl)
  Domain.Fabric -> do
    base <- asks (.fabricGatewayBaseUrl)
    pure $ base {baseUrlPath = baseUrlPath base <> "/bap/caller/" <> T.unpack merchant.bapId}

-- Fabric layer gets automatically mutated and signed context from Onix layer , this handling is done internally 
-- for reference 
-- ┌──────────────────────────────────────────────────────────────────────────┐
-- │ onix-bap  /bap/caller/select                                             │
-- │   1. reqpreprocessor middleware extracts txn/msg IDs                     │
-- │   2. addRoute        (BAPCaller.yaml → onix-bpp:8082/bpp/receiver)       │
-- │   3. storePayload    (Redis)                                             │
-- │   4. validateSchema  (v2.0.0 core + extended mobility contexts)          │
-- │   5. sign            (BAP private key)                                   │
-- │   6. POST outbound; validateAckSign on the sync response                 │
-- └──────────────────────────────────────────────────────────────────────────┘
dispatchToGateways ::
  ( MonadFlow m,
    Metrics.CoreMetrics m,
    ToJSON req,
    HasFlowEnv
      m
      r
      '[ "ondcGatewayUrl" ::: BaseUrl,
         "nyGatewayUrl" ::: BaseUrl,
         "fabricGatewayBaseUrl" ::: BaseUrl
       ]
  ) =>
  DM.Merchant ->
  Maybe DBC.BecknConfig ->
  -- | V1 action name as we emit it internally (e.g. "search", "select").
  -- For Fabric, both `context.action` and the URL segment are rewritten via
  -- `fabricActionName` (search -> discover, on_search -> on_discover, others
  -- pass-through) so onix routes to the correct V1<->V2 mapping in
  -- mobility-mappings.yaml.
  Text ->
  req ->
  (BaseUrl -> req -> m ()) ->
  -- | Unsigned callback receives (URL, fabric-mapped action, mutated JSON body).
  (BaseUrl -> Text -> A.Value -> m ()) ->
  m ()
dispatchToGateways merchant mbBapConfig action req signedCall unsignedCall = do
  let targets = case merchant.gatewayDispatchGroups of
        Just (grp : _) -> grp
        _ -> take 1 merchant.gatewayAndRegistryPriorityList
  forM_ targets $ \gw -> do
    url <- resolveGatewayUrl merchant gw
    case gw of
      Domain.Fabric -> do
        base <- asks (.fabricGatewayBaseUrl)
        let bapReceiverUri = showBaseUrl $ base {baseUrlPath = baseUrlPath base <> "/bap/receiver/" <> T.unpack merchant.bapId}
            jsonReq = A.toJSON req
            mutated = maybe jsonReq (\nid -> BecknContext.mutateFabricContext nid bapReceiverUri jsonReq) (mbBapConfig >>= (.networkId))
            mappedAction = BecknContext.fabricActionName action
        Metrics.incrementGenericMetrics $ "fabric_outbound_" <> mappedAction
        unsignedCall url mappedAction mutated
      _ -> signedCall url req
