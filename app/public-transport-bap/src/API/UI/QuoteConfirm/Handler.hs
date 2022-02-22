module API.UI.QuoteConfirm.Handler where

import Beckn.Prelude
import Beckn.Types.Core.ReqTypes (BecknReq (BecknReq))
import Beckn.Types.Id
import Beckn.Utils.Common
import Core.ACL.Confirm
import Core.Context
import Core.Spec.Common.Context
import Core.Spec.Confirm (ConfirmMessage (ConfirmMessage))
import Domain.Action.UI.QuoteConfirm
import qualified Domain.Types.Quote as D
import Environment
import qualified ExternalAPI.Flow as ExternalAPI
import Tools.Auth (PersonId)

handler :: PersonId -> Id D.Quote -> QConfirmReq -> FlowHandler QConfirmRes
handler personId quoteId confirmReq = withFlowHandlerAPI $ do
  (res, becknReq) <- quoteConfirm personId quoteId confirmReq
  callConfirm becknReq
  pure res

callConfirm :: ConfirmMessageD -> Flow ()
callConfirm msg = do
  selfUrl <- askConfig (.selfURI)
  selfId <- askConfig (.selfId)
  context <- buildContext CONFIRM msg.txnId selfId selfUrl Nothing Nothing
  let confirmOrder = mkConfirmMessage msg
  ExternalAPI.confirm msg.booking.bppUrl (BecknReq context $ ConfirmMessage confirmOrder)
