module IssueManagement.SharedLogic.CallAPI where

import qualified BecknV2.IGM.APIs as API
import qualified Data.HashMap.Strict as HMS
import qualified EulerHS.Types as ET
import IGM.Enums as Spec
import IGM.Types as Spec
import IssueManagement.Common
import qualified IssueManagement.Common as Common
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth
import TransactionLogs.Interface.Types (KeyConfig, TokenConfig)
import TransactionLogs.PushLogs

callOnIssue ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    MonadReader r m
  ) =>
  Spec.OnIssueReq ->
  BaseUrl ->
  Common.Merchant ->
  m ()
callOnIssue req domainUri merchant = do
  let bppSubscriberId = getShortId $ merchant.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  fork "IGM on_issue pushing ondc logs" . void $
    pushLogs (show Spec.ON_ISSUE) (toJSON req) merchant.id.getId "MOBILITY"
  void $
    withShortRetry $
      Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Spec.ON_ISSUE) API.onIssueAPI domainUri internalEndPointHashMap req

callOnIssueStatus ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    MonadReader r m
  ) =>
  Spec.OnIssueStatusReq ->
  BaseUrl ->
  Merchant ->
  m ()
callOnIssueStatus req domainUri merchant = do
  let bppSubscriberId = getShortId merchant.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  fork "IGM on_issue_status pushing ondc logs" . void $
    pushLogs (show Spec.ON_ISSUE_STATUS) (toJSON req) merchant.id.getId "MOBILITY"
  void $
    withShortRetry $
      Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Spec.ON_ISSUE_STATUS) API.onIssueStatusAPI domainUri internalEndPointHashMap req
