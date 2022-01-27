module API.UI.QuoteConfirm.Handler where

import API.UI.QuoteConfirm.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Core.Migration.Context
import Beckn.Types.Core.ReqTypes (BecknReq (BecknReq))
import Beckn.Types.Id
import Beckn.Utils.Common
import Core.ACL.Confirm
import Core.Spec.Confirm (ConfirmMessage (ConfirmMessage))
<<<<<<< HEAD
import Domain.Endpoints.UI.QuoteConfirm
=======
import Domain.Confirm (ConfirmMessageD)
import Domain.Endpoints.UI.QuoteConfirm.Handler
>>>>>>> Added confirm/on_confirm for public transport bap
import qualified Domain.Types.Quote as D
import qualified ExternalAPI.Flow as ExternalAPI
import Tools.Auth (PersonId)
import Tools.Context

handler :: PersonId -> Id D.Quote -> QConfirmReq -> FlowHandler QConfirmRes
handler personId quoteId confirmReq = withFlowHandlerAPI $ do
  (res, becknReq) <- quoteConfirm personId quoteId confirmReq
  callConfirm becknReq
  pure res

callConfirm :: ConfirmMessageD -> Flow ()
callConfirm msg = do
<<<<<<< HEAD
  selfUrl <- askConfig (.selfURI)
  selfId <- askConfig (.selfId)
  context <- buildContext CONFIRM msg.txnId selfUrl Nothing selfId
=======
  selfUrl <- asks (.config.selfURI)
  selfId <- asks (.config.selfId)
  context <- buildContext CONFIRM msg.txnId selfId selfUrl Nothing
>>>>>>> Added confirm/on_confirm for public transport bap
  let confirmOrder = mkConfirmMessage msg
  ExternalAPI.confirm msg.booking.bppUrl (BecknReq context $ ConfirmMessage confirmOrder)
