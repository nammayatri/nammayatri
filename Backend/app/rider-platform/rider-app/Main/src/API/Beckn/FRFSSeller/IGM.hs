module API.Beckn.FRFSSeller.IGM (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.IGM.APIs as Spec
import qualified Domain.Action.Beckn.FRFSSeller.IGM as DIGM
import Environment
import qualified IGM.Types as IGMSpec
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)

type API =
  Spec.IssueAPI
    :<|> Spec.IssueStatusAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler operator auth = issue operator auth :<|> issueStatus operator auth

igmAck :: IGMSpec.AckResponse
igmAck =
  IGMSpec.AckResponse
    { ackResponseError = Nothing,
      ackResponseMessage =
        IGMSpec.AckMessage
          { ackMessageAck = IGMSpec.Ack {ackStatus = Just "ACK"}
          }
    }

issue :: Text -> SignatureAuthResult -> IGMSpec.IssueReq -> FlowHandler IGMSpec.AckResponse
issue operator _authResult req =
  withFlowHandlerAPI $ do
    H.claimOnce
      operator
      "issue"
      req.context.contextTransactionId
      req.context.contextMessageId
      (DIGM.handleIssue operator req)
    pure igmAck

issueStatus :: Text -> SignatureAuthResult -> IGMSpec.IssueStatusReq -> FlowHandler IGMSpec.AckResponse
issueStatus operator _authResult req =
  withFlowHandlerAPI $ do
    H.claimOnce
      operator
      "issue_status"
      req.issueStatusReqContext.contextTransactionId
      req.issueStatusReqContext.contextMessageId
      (DIGM.handleIssueStatus operator req)
    pure igmAck
