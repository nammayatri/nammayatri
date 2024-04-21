{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallFRFSBPP where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Merchant as Merchant
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth
import Tools.Metrics (CoreMetrics)
import TransactionLogs.PushLogs
import TransactionLogs.Types

search ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  Spec.SearchReq ->
  Id Merchant.Merchant ->
  m Spec.AckResponse
search gatewayUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.searchReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "search" Spec.searchAPI gatewayUrl internalEndPointHashMap req

init ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  Spec.InitReq ->
  Id Merchant.Merchant ->
  m Spec.AckResponse
init providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.initReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "init" Spec.initAPI providerUrl internalEndPointHashMap req

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  Spec.ConfirmReq ->
  Id Merchant.Merchant ->
  m Spec.AckResponse
confirm providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.confirmReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "confirm" Spec.confirmAPI providerUrl internalEndPointHashMap req

status ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  Spec.StatusReq ->
  Id Merchant.Merchant ->
  m Spec.AckResponse
status providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.statusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "status" Spec.statusAPI providerUrl internalEndPointHashMap req

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  Spec.CancelReq ->
  Id Merchant.Merchant ->
  m Spec.AckResponse
cancel providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.cancelReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "cancel" Spec.cancelAPI providerUrl internalEndPointHashMap req

callBecknAPIWithSignature' ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    ToJSON req,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  Id Merchant.Merchant ->
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature' merchantId a b c d e req' = do
  fork ("sending " <> show b <> ", pushing ondc logs") $ do
    void $ pushLogs b (toJSON req') merchantId.getId
  callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing b c d e req'
