module API.UI.Select
  ( DSelect.DSelectRes (..),
    DSelect.SelectListRes (..),
    API,
    handler,
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Core.ACL.Select as ACL
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified ExternalAPI.Flow as ExternalAPI
import Servant
import Utils.Auth
import Utils.Common

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
       )

handler :: FlowServer API
handler =
  select
    :<|> selectList

select :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
select personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq
  void $ ExternalAPI.select dSelectReq.providerUrl becknReq
  pure Success

selectList :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler DSelect.SelectListRes
selectList personId = withFlowHandlerAPI . withPersonIdLogTag personId . DSelect.selectList
