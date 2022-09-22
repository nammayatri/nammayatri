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
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignServerAccess" --TODO resetServerAccess
             :> ReqBody '[JSON] DPerson.ServerAccessReq
             :> Post '[JSON] APISuccess
       )
    :<|> "person"
      :> ( "profile"
             :> DashboardAuth 'DASHBOARD_USER
             :> Get '[JSON] DP.PersonAPIEntity
             :<|> "getCurrentServer"
               :> DashboardAuth 'DASHBOARD_USER
               :> Get '[JSON] DPerson.ServerAccessRes
         )

handler :: FlowServer API
handler =
  ( listPerson
      :<|> assignRole
      :<|> assignServerAccess
  )
    :<|> ( profile
             :<|> getCurrentServer
         )

listPerson :: TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPersonRes
listPerson tokenInfo mbSearchString mbLimit =
  withFlowHandlerAPI . DPerson.listPerson tokenInfo mbSearchString mbLimit

assignRole :: TokenInfo -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole tokenInfo personId =
  withFlowHandlerAPI . DPerson.assignRole tokenInfo personId

assignServerAccess :: TokenInfo -> Id DP.Person -> DPerson.ServerAccessReq -> FlowHandler APISuccess
assignServerAccess tokenInfo personId =
  withFlowHandlerAPI . DPerson.assignServerAccess tokenInfo personId

profile :: TokenInfo -> FlowHandler DP.PersonAPIEntity
profile =
  withFlowHandlerAPI . DPerson.profile

getCurrentServer :: TokenInfo -> FlowHandler DPerson.ServerAccessRes
getCurrentServer =
  withFlowHandlerAPI . pure . DPerson.getCurrentServer
