{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallIGMBPP where

import qualified BecknV2.IGM.APIs as Spec
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Merchant as Merchant
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import qualified IGM.Types as IGM
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

issue ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r,
    ToJSON IGM.IssueReq,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  BaseUrl ->
  Id Merchant.Merchant ->
  IGM.IssueReq ->
  m IGM.AckResponse
issue providerUrl merchantId req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "issue" Spec.issueAPI providerUrl internalEndPointHashMap req

onIssue ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r,
    ToJSON IGM.OnIssueReq,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  BaseUrl ->
  Id Merchant.Merchant ->
  IGM.OnIssueReq ->
  m IGM.AckResponse
onIssue providerUrl merchantId req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bppId <- req.onIssueReqContext.contextBppId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bppId "onIssue" Spec.onIssueAPI providerUrl internalEndPointHashMap req

issueStatus ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r,
    ToJSON IGM.IssueStatusReq,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  BaseUrl ->
  Id Merchant.Merchant ->
  IGM.IssueStatusReq ->
  m IGM.AckResponse
issueStatus providerUrl merchantId req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.issueStatusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature' merchantId bapId "issueStatus" Spec.issueStatusAPI providerUrl internalEndPointHashMap req

callBecknAPIWithSignature' ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    ToJSON req,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasRequestId r
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
  fork ("sending " <> show b <> ", pushing ondc logs") do
    void $ pushLogs b (toJSON req') merchantId.getId "MOBILITY"
  callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing b c d e req'
