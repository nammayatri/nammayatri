module API.UI.Select
  ( DSelect.DSelectRes (..),
    DSelect.SelectListRes (..),
    API,
    handler,
  )
where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.ACL.Cancel as CACL
import qualified Core.ACL.Select as ACL
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import Environment
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Tools.Auth

-------- Select Flow --------
type API =
  "estimate"
    :> ( TokenAuth
           :> Capture "estimateId" (Id DEstimate.Estimate)
           :> "select"
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
    :<|> selectList
    :<|> cancelSearch

select :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
select personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq
  void $ withShortRetry $ CallBPP.select dSelectReq.providerUrl becknReq
  pure Success

selectList :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectList

cancelSearch :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
cancelSearch personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dCancelRes <- DCancel.cancelSearch personId estimateId
  void $ withShortRetry $ CallBPP.cancel dCancelRes.providerUrl =<< CACL.buildCancelSearchReq dCancelRes
  pure Success