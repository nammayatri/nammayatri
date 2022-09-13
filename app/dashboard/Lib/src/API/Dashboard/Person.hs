module API.Dashboard.Person where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import qualified Domain.Action.Dashboard.Person as DPerson
import Domain.Types.Person as DP
import Environment
import Servant hiding (Unauthorized, throwError)
import Tools.Auth
import Tools.Roles.Instances

type API =
  "person"
    :> "list"
    :> TokenAuth (DashboardAccessLevel 'DASHBOARD_ADMIN)
    :> QueryParam "searchString" Text
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] DPerson.ListPersonRes

handler :: FlowServer API
handler = listPerson

listPerson :: Id DP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPersonRes
listPerson personId mbSearchString mbLimit =
  withFlowHandlerAPI . DPerson.listPerson personId mbSearchString mbLimit
