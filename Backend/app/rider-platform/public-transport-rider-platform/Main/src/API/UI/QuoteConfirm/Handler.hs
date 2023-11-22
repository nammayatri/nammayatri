{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.QuoteConfirm.Handler where

import Beckn.ACL.Confirm
import Beckn.Context
import Beckn.Spec.Common.Context
import Beckn.Spec.Confirm (ConfirmMessage (ConfirmMessage))
import Domain.Action.UI.QuoteConfirm
import qualified Domain.Types.Quote as D
import Environment
import qualified ExternalAPI.Flow as ExternalAPI
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes (BecknReq (BecknReq))
import Kernel.Types.Id
import Kernel.Utils.Common hiding (buildContext)
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
