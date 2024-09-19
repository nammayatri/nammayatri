module API.Beckn.IGM.OnIssue where

-- import qualified IssueManagement.Beckn.ACL.Issue as ACL
-- import API.UI.Issue (driverIssueHandle)
-- import qualified BecknV2.IGM.APIs as Spec

-- import qualified Domain.Types.Merchant as DM
import Environment
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
-- import qualified Kernel.Types.Beckn.Domain as Domain

-- import Servant hiding (throwError)

-- import qualified IssueManagement.API.Beckn.Issue as BI
import qualified IssueManagement.Beckn.ACL.OnIssue as ACL
-- import qualified IssueManagement.Common as Common
-- import qualified IssueManagement.Domain.Action.Beckn.Issue as DIssue

-- import Kernel.Types.Id

-- import qualified SharedLogic.CallIGMBAP as CallBAP
import qualified IssueManagement.Domain.Action.Beckn.OnIssue as DOnIssue
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Storage.Beam.IssueManagement ()

onIssue ::
  SignatureAuthResult ->
  Spec.OnIssueReq ->
  FlowHandler Spec.AckResponse
onIssue _ req = withFlowHandlerAPI $ do
  transaction_id <- req.onIssueReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnIssue request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    message_id <- req.onIssueReqContext.contextMessageId & fromMaybeM (InvalidRequest "MessageId not found")
    onIssueReq <- ACL.buildOnIssueReq req
    Redis.whenWithLockRedis (onIssueLockKey message_id) 60 $ do
      issue <- DOnIssue.validateRequest onIssueReq
      fork "IGM on_issue processing" $ do
        Redis.whenWithLockRedis (onIssueProcessingLockKey message_id) 60 $
          DOnIssue.handler onIssueReq issue
  pure Utils.ack

onIssueLockKey :: Text -> Text
onIssueLockKey id = "IGM:OnIssue:MessageId-" <> id

onIssueProcessingLockKey :: Text -> Text
onIssueProcessingLockKey id = "IGM:OnIssue:Processing:MessageId-" <> id
