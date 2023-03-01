{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Select
  ( DSelect.DSelectReq (..),
    DSelect.DEstimateSelectReq (..),
    DSelect.DSelectRes (..),
    DSelect.SelectListRes (..),
    API,
    handler,
  )
where

import qualified Beckn.ACL.Cancel as CACL
import qualified Beckn.ACL.Select as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Tools.Auth

-------- Select Flow --------
type API =
  "estimate"
    :> ( TokenAuth
           :> Capture "estimateId" (Id DEstimate.Estimate)
           :> "select"
           :> ReqBody '[JSON] DSelect.DSelectReq
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "select2" -- TODO will replace "select" once 100% rolled out
             :> ReqBody '[JSON] DSelect.DEstimateSelectReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "quotes"
             :> Get '[JSON] DSelect.SelectListRes
           :<|> TokenAuth
             :> Capture "estimateId" (Id DEstimate.Estimate)
             :> "cancel"
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  select
    :<|> select2
    :<|> selectList
    :<|> cancelSearch

select :: Id DPerson.Person -> Id DEstimate.Estimate -> DSelect.DSelectReq -> FlowHandler APISuccess
select personId estimateId req' = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let req = DSelect.DEstimateSelectReq {customerExtraFee = req'.customerExtraFee, autoAssignEnabled = False}
  dSelectReq <- DSelect.select personId estimateId req
  becknReq <- ACL.buildSelectReq dSelectReq
  void $ withShortRetry $ CallBPP.select dSelectReq.providerUrl becknReq
  pure Success

select2 :: Id DPerson.Person -> Id DEstimate.Estimate -> DSelect.DEstimateSelectReq -> FlowHandler APISuccess
select2 personId estimateId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId estimateId req
  becknReq <- ACL.buildSelectReq dSelectReq
  void $ withShortRetry $ CallBPP.select dSelectReq.providerUrl becknReq
  pure Success

selectList :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectList

cancelSearch :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
cancelSearch personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dCancelRes <- DCancel.cancelSearch personId estimateId
  when (dCancelRes.sendToBpp) $
    void $ withShortRetry $ CallBPP.cancel dCancelRes.providerUrl =<< CACL.buildCancelSearchReq dCancelRes
  pure Success
