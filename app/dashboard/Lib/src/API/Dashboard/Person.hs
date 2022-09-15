module API.Dashboard.Person where

import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Environment
import Servant hiding (Unauthorized, throwError)
import Tools.Auth

type API =
  "person"
    :> ( "list"
           :> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
           :> QueryParam "searchString" Text
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] DPerson.ListPersonRes
           :<|> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
           :> Capture "personId" (Id DP.Person)
           :> "assignRole"
           :> Capture "roleId" (Id DRole.Role)
           :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  listPerson
    :<|> assignRole

listPerson :: Id DP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPersonRes
listPerson adminId mbSearchString mbLimit =
  withFlowHandlerAPI . DPerson.listPerson adminId mbSearchString mbLimit

assignRole :: Id DP.Person -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole adminId personId =
  withFlowHandlerAPI . DPerson.assignRole adminId personId
