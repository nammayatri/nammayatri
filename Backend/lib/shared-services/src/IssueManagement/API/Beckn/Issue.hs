module IssueManagement.API.Beckn.Issue where

import qualified Data.HashMap.Strict as HMS
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import qualified IssueManagement.Beckn.ACL.Issue as IACL
import qualified IssueManagement.Beckn.ACL.IssueStatus as ISACL
import qualified IssueManagement.Beckn.ACL.OnIssue as OIACL
import qualified IssueManagement.Beckn.ACL.OnIssueStatus as OISACL
import IssueManagement.Common
import qualified IssueManagement.Common.Beckn.Issue as Common
import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
import qualified IssueManagement.Domain.Action.Beckn.IssueStatus as DIssueStatus
import qualified IssueManagement.Domain.Action.Beckn.OnIssue as DOnIssue
import qualified IssueManagement.Domain.Action.Beckn.OnIssueStatus as DOnIssueStatus
import IssueManagement.Domain.Action.UI.Issue
import qualified IssueManagement.SharedLogic.CallAPI as CallAPI
import IssueManagement.Storage.BeamFlow
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (Unauthorized, throwError)
import Slack.Types (SlackNotificationConfig)

type IssueAPI =
  Common.OnDemandAPI
    :<|> Common.PublicTransportAPI

issue ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    HasField "slackNotificationConfig" r SlackNotificationConfig,
    EncFlow m r
  ) =>
  Id Merchant ->
  Spec.IssueReq ->
  ServiceHandle m ->
  Identifier ->
  m Spec.AckResponse
issue merchantId req issueHandle identifier = do
  transaction_id <- req.context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bap_id <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  bpp_id <- req.context.contextBppId & fromMaybeM (InvalidRequest "BppId not found")
  bap_uri <- req.context.contextBapUri & fromMaybeM (InvalidRequest "BapUri not found")
  bpp_uri <- req.context.contextBppUri & fromMaybeM (InvalidRequest "BppUri not found")
  logDebug $ "Received Issue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    issueReq <- IACL.buildIssueReq req
    Redis.whenWithLockRedis (issueLockKey message_id) 60 $ do
      validatedIssueReq <- DIssue.validateRequest merchantId issueReq issueHandle
      fork "IGM issue processing" $ do
        Redis.whenWithLockRedis (issueProcessingLockKey message_id) 60 $ do
          dIssueRes <- DIssue.handler validatedIssueReq issueHandle
          let (domainId, domainUri) = if identifier == DRIVER then (bap_id, bap_uri) else (bpp_id, bpp_uri)
          becknOnIssueReq <- IACL.buildOnIssueReq transaction_id message_id domainId domainUri dIssueRes
          domainBaseUrl <- parseBaseUrl domainUri
          void $ CallAPI.callOnIssue becknOnIssueReq domainBaseUrl dIssueRes.merchant
  pure Utils.ack

issueLockKey :: Text -> Text
issueLockKey message_id = "IGM:Issue:MessageId" <> message_id

issueProcessingLockKey :: Text -> Text
issueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id

onIssue ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    BeamFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id Merchant ->
  Spec.OnIssueReq ->
  ServiceHandle m ->
  Identifier ->
  m Spec.AckResponse
onIssue _merchantId req _ _identifier = do
  transaction_id <- req.onIssueReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnIssue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onIssueReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    onIssueReq <- OIACL.buildOnIssueReq req
    Redis.whenWithLockRedis (onIssueLockKey message_id) 60 $ do
      issue' <- DOnIssue.validateRequest onIssueReq
      fork "IGM on_issue processing" $ do
        Redis.whenWithLockRedis (onIssueProcessingLockKey message_id) 60 $
          DOnIssue.handler onIssueReq issue'
  pure Utils.ack

onIssueLockKey :: Text -> Text
onIssueLockKey id = "IGM:OnIssue:MessageId-" <> id

onIssueProcessingLockKey :: Text -> Text
onIssueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id

issueStatus ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    BeamFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id Merchant ->
  Spec.IssueStatusReq ->
  ServiceHandle m ->
  Identifier ->
  m Spec.AckResponse
issueStatus _merchantId req issueHandle identifier = do
  transaction_id <- req.issueStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.issueStatusReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bap_id <- req.issueStatusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  bpp_id <- req.issueStatusReqContext.contextBppId & fromMaybeM (InvalidRequest "BppId not found")
  bap_uri <- req.issueStatusReqContext.contextBapUri & fromMaybeM (InvalidRequest "BapUri not found")
  bpp_uri <- req.issueStatusReqContext.contextBppUri & fromMaybeM (InvalidRequest "BppUri not found")
  logDebug $ "Received IssueStatus request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    issueStatusReq <- ISACL.buildIssueStatusReq req
    Redis.whenWithLockRedis (issueStatusLockKey message_id) 60 $ do
      validatedIssueStatusReq <- DIssueStatus.validateRequest issueStatusReq issueHandle
      fork "IGM issueStatus processing" $ do
        Redis.whenWithLockRedis (issueStatusProcessingLockKey message_id) 60 $ do
          dIssueStatusRes <- DIssueStatus.handler validatedIssueStatusReq
          let (domainId, domainUri) = if identifier == DRIVER then (bap_id, bap_uri) else (bpp_id, bpp_uri)
          becknOnIssueReq <- ISACL.buildOnIssueStatusReq transaction_id message_id domainId domainUri dIssueStatusRes
          domainBaseUrl <- parseBaseUrl domainUri
          void $ CallAPI.callOnIssueStatus becknOnIssueReq domainBaseUrl dIssueStatusRes.merchant
  return Utils.ack

issueStatusLockKey :: Text -> Text
issueStatusLockKey message_id = "IGM:IssueStatus:MessageId" <> message_id

issueStatusProcessingLockKey :: Text -> Text
issueStatusProcessingLockKey id = "IGM:OnIssueStatus:Processing:MessageId-" <> id

onIssueStatus ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    BeamFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id Merchant ->
  Spec.OnIssueStatusReq ->
  ServiceHandle m ->
  Identifier ->
  m Spec.AckResponse
onIssueStatus _merchantId req _ _identifier = do
  transaction_id <- req.onIssueStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnIssueStatus request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onIssueStatusReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    onIssueStatusReq <- OISACL.buildOnIssueStatusReq req
    Redis.whenWithLockRedis (onIssueStatusLockKey message_id) 60 $ do
      issue' <- DOnIssueStatus.validateRequest onIssueStatusReq
      fork "IGM on_issue processing" $ do
        Redis.whenWithLockRedis (onIssueStatusProcessingLockKey message_id) 60 $
          DOnIssueStatus.onIssueStatus onIssueStatusReq issue'
  pure Utils.ack

onIssueStatusLockKey :: Text -> Text
onIssueStatusLockKey id = "IGM:OnIssueStatus:MessageId-" <> id

onIssueStatusProcessingLockKey :: Text -> Text
onIssueStatusProcessingLockKey id = "IGM:OnIssueStatus:Processing:MessageId-" <> id
