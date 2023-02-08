module API.UI.QuoteConfirm.Handler where

import Core.ACL.Confirm
import Core.Context
import Core.Spec.Common.Context
import Core.Spec.Confirm (ConfirmMessage (ConfirmMessage))
import Domain.Action.UI.QuoteConfirm
import qualified Domain.Types.Quote as D
import Environment
import qualified ExternalAPI.Flow as ExternalAPI
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes (BecknReq (BecknReq))
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Auth (PersonId)

handler :: PersonId -> Id D.Quote -> QConfirmReq -> FlowHandler QConfirmRes
handler personId quoteId confirmReq = withFlowHandlerAPI $ do
  (res, becknReq) <- quoteConfirm personId quoteId confirmReq
  callConfirm becknReq
  pure res

callConfirm :: ConfirmMessageD -> Flow ()
callConfirm msg = do
  selfUrl <- asks (.selfURI)
  selfId <- asks (.selfId)
  context <- buildContext CONFIRM msg.txnId selfId selfUrl Nothing Nothing
  let confirmOrder = mkConfirmMessage msg
  ExternalAPI.confirm msg.booking.bppUrl (BecknReq context $ ConfirmMessage confirmOrder)
