module IssueManagement.API.Beckn.Issue where

-- import qualified BecknV2.IGM.APIs as Spec

-- import qualified Kernel.Types.Beckn.Domain as Domain

-- import Servant hiding (throwError)

-- import qualified IssueManagement.Beckn.ACL.OnIssueStatus as ACL

-- import qualified Domain.Action.Beckn.IGM.IssueStatus as DIssueStatus
-- import Environment
-- import qualified IGM.Types as Spec
-- import qualified IGM.Utils as Utils
-- import Kernel.Prelude
-- import qualified Kernel.Storage.Hedis as Redis

-- import Kernel.Types.Error

-- import Kernel.Utils.Common

-----

import qualified Data.HashMap.Strict as HMS
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import qualified IssueManagement.Beckn.ACL.Issue as ACL
import qualified IssueManagement.Beckn.ACL.IssueStatus as ACL
import IssueManagement.Common
import qualified IssueManagement.Common.Beckn.Issue as Common
import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue
import qualified IssueManagement.Domain.Action.Beckn.IssueStatus as DIssueStatus
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

type IssueAPI =
  "beckn"
    :> Common.IssueAPI
    :<|> Common.IssueStatusAPI

issue ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    BeamFlow m r,
    EsqDBReplicaFlow m r,
    HasField "sosAlertsTopicARN" r Text,
    EncFlow m r
  ) =>
  Id Merchant ->
  Spec.IssueReq ->
  ServiceHandle m ->
  Identifier ->
  m Spec.AckResponse
issue merchantId req issueHandle _identifier = do
  transaction_id <- req.context.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.context.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bap_id <- req.context.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  bap_uri <- req.context.contextBapUri & fromMaybeM (InvalidRequest "BapUri not found")
  logDebug $ "Received Issue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    issueReq <- ACL.buildIssueReq req
    Redis.whenWithLockRedis (issueLockKey message_id) 60 $ do
      validatedIssueReq <- DIssue.validateRequest merchantId issueReq issueHandle
      fork "IGM issue processing" $ do
        Redis.whenWithLockRedis (issueProcessingLockKey message_id) 60 $ do
          dIssueRes <- DIssue.handler validatedIssueReq issueHandle
          becknOnIssueReq <- ACL.buildOnIssueReq transaction_id message_id bap_id bap_uri dIssueRes
          bapUrl <- parseBaseUrl bap_uri
          void $ CallAPI.callOnIssue becknOnIssueReq bapUrl dIssueRes.merchant
  pure Utils.ack

issueLockKey :: Text -> Text
issueLockKey message_id = "IGM:Issue:MessageId" <> message_id

issueProcessingLockKey :: Text -> Text
issueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id

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
issueStatus _merchantId req issueHandle _identifier = do
  transaction_id <- req.issueStatusReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  message_id <- req.issueStatusReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
  bap_id <- req.issueStatusReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId not found")
  bap_uri <- req.issueStatusReqContext.contextBapUri & fromMaybeM (InvalidRequest "BapUri not found")
  logDebug $ "Received IssueStatus request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    issueStatusReq <- ACL.buildIssueStatusReq req
    Redis.whenWithLockRedis (issueStatusLockKey message_id) 60 $ do
      validatedIssueStatusReq <- DIssueStatus.validateRequest issueStatusReq issueHandle
      fork "IGM issueStatus processing" $ do
        Redis.whenWithLockRedis (issueStatusProcessingLockKey message_id) 60 $ do
          dIssueStatusRes <- DIssueStatus.handler validatedIssueStatusReq
          becknOnIssueReq <- ACL.buildOnIssueStatusReq transaction_id message_id bap_id bap_uri dIssueStatusRes
          bapUrl <- parseBaseUrl bap_uri
          void $ CallAPI.callOnIssueStatus becknOnIssueReq bapUrl dIssueStatusRes.merchant
  return Utils.ack

issueStatusLockKey :: Text -> Text
issueStatusLockKey message_id = "IGM:IssueStatus:MessageId" <> message_id

issueStatusProcessingLockKey :: Text -> Text
issueStatusProcessingLockKey id = "IGM:OnIssueStatus:Processing:MessageId-" <> id
