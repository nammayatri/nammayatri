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
  "admin"
    :> "person"
    :> ( "list"
           :> DashboardAuth 'DASHBOARD_ADMIN
           :> QueryParam "searchString" Text
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] DPerson.ListPersonRes
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignRole"
             :> Capture "roleId" (Id DRole.Role)
             :> Post '[JSON] APISuccess
       )
    :<|> "person"
      :> "profile"
      :> DashboardAuth 'DASHBOARD_USER
      :> Get '[JSON] DP.PersonAPIEntity

handler :: FlowServer API
handler =
  ( listPerson
      :<|> assignRole
  )
    :<|> profile

listPerson :: Id DP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPersonRes
listPerson adminId mbSearchString mbLimit =
  withFlowHandlerAPI . DPerson.listPerson adminId mbSearchString mbLimit

assignRole :: Id DP.Person -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole adminId personId =
  withFlowHandlerAPI . DPerson.assignRole adminId personId

profile :: Id DP.Person -> FlowHandler DP.PersonAPIEntity
profile =
  withFlowHandlerAPI . DPerson.profile
