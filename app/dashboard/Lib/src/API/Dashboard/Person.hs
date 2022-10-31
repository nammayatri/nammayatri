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
             :> "assignMerchantAccess"
             :> ReqBody '[JSON] DPerson.MerchantAccessReq
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "resetMerchantAccess"
             :> ReqBody '[JSON] DPerson.MerchantAccessReq
             :> Post '[JSON] APISuccess
       )
    :<|> "person"
      :> ( "profile"
             :> DashboardAuth 'DASHBOARD_USER
             :> Get '[JSON] DP.PersonAPIEntity
             :<|> "getCurrentMerchant"
               :> DashboardAuth 'DASHBOARD_USER
               :> Get '[JSON] DPerson.MerchantAccessRes
             :<|> DashboardAuth 'DASHBOARD_USER
               :> "changePassword"
               :> ReqBody '[JSON] DPerson.ChangePasswordReq
               :> Post '[JSON] APISuccess
         )

handler :: FlowServer API
handler =
  ( listPerson
      :<|> assignRole
      :<|> assignMerchantAccess
      :<|> resetMerchantAccess
  )
    :<|> ( profile
             :<|> getCurrentMerchant
             :<|> changePassword
         )

listPerson :: TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DPerson.ListPersonRes
listPerson tokenInfo mbSearchString mbLimit =
  withFlowHandlerAPI . DPerson.listPerson tokenInfo mbSearchString mbLimit

assignRole :: TokenInfo -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole tokenInfo personId =
  withFlowHandlerAPI . DPerson.assignRole tokenInfo personId

assignMerchantAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
assignMerchantAccess tokenInfo personId =
  withFlowHandlerAPI . DPerson.assignMerchantAccess tokenInfo personId

resetMerchantAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
resetMerchantAccess tokenInfo personId =
  withFlowHandlerAPI . DPerson.resetMerchantAccess tokenInfo personId

profile :: TokenInfo -> FlowHandler DP.PersonAPIEntity
profile =
  withFlowHandlerAPI . DPerson.profile

getCurrentMerchant :: TokenInfo -> FlowHandler DPerson.MerchantAccessRes
getCurrentMerchant =
  withFlowHandlerAPI . DPerson.getCurrentMerchant

changePassword :: TokenInfo -> DPerson.ChangePasswordReq -> FlowHandler APISuccess
changePassword req =
  withFlowHandlerAPI . DPerson.changePassword req
