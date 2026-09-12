{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Person where

import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Action.Dashboard.Roles as DRoles
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Flow (FlowR)
import Kernel.Types.Id
import Kernel.Utils.Common (FlowHandlerR, FlowServerR, fromMaybeM)
import Servant hiding (Unauthorized, throwError)
import qualified Storage.Queries.Merchant as QMerchant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)
import Tools.Error

type API =
  "admin"
    :> "person"
    :> ( "list"
           :> DashboardAuth 'DASHBOARD_ADMIN
           :> QueryParam "searchString" Text
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> QueryParam "personId" (Id DP.Person)
           :> Get '[JSON] DPerson.ListPersonRes
           :<|> "ptList"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> QueryParam "searchString" Text
             :> QueryParam "roleName" Text
             :> QueryParam "entityShortId" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DPerson.ListPTEmployeeRes
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignRole"
             :> Capture "roleId" (Id DRole.Role)
             :> Post '[JSON] APISuccess
           -- TODO : Deprecated, Remove after successful deployment
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignMerchantAccess"
             :> ReqBody '[JSON] DPerson.MerchantAccessReq
             :> Post '[JSON] APISuccess
           -- End of Deprecated API.
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "assignMerchantCityAccess"
             :> ReqBody '[JSON] DPerson.MerchantCityAccessReq
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> "resetMerchantAccess"
             :> ReqBody '[JSON] DPerson.MerchantAccessReq
             :> Post '[JSON] APISuccess
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personid" (Id DP.Person)
             :> "resetMerchantCityAccess"
             :> ReqBody '[JSON] DPerson.MerchantCityAccessReq
             :> Post '[JSON] APISuccess
           :<|> "create"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> ReqBody '[JSON] DPerson.CreatePersonReq
             :> Post '[JSON] DPerson.CreatePersonRes
           :<|> "delete"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> QueryParam "deleteReason" Text
             :> Delete '[JSON] APISuccess
           :<|> "changeEnabledStatus"
             :> DashboardAuth 'DASHBOARD_ADMIN
             :> Capture "personId" (Id DP.Person)
             :> ReqBody '[JSON] DPerson.ChangeEnabledStatusReq
             :> Post '[JSON] APISuccess
           :<|> ( "change"
                    :> "email"
                    :> DashboardAuth 'DASHBOARD_ADMIN
                    :> Capture "personId" (Id DP.Person)
                    :> ReqBody '[JSON] DPerson.ChangeEmailByAdminReq
                    :> Post '[JSON] APISuccess
                    :<|> "password"
                    :> DashboardAuth 'DASHBOARD_ADMIN
                    :> Capture "personId" (Id DP.Person)
                    :> ReqBody '[JSON] DPerson.ChangePasswordByAdminReq
                    :> Post '[JSON] APISuccess
                    :<|> "mobile"
                    :> DashboardAuth 'DASHBOARD_ADMIN
                    :> Capture "personId" (Id DP.Person)
                    :> ReqBody '[JSON] DPerson.ChangeMobileNumberByAdminReq
                    :> Post '[JSON] APISuccess
                )
       )
    :<|> "user"
      :> ( "profile"
             :> DashboardAuth 'DASHBOARD_USER
             :> Get '[JSON] DP.PersonAPIEntity
             :<|> "updateProfile"
               :> DashboardAuth 'DASHBOARD_USER
               :> ReqBody '[JSON] DPerson.UpdateProfileReq
               :> Put '[JSON] APISuccess
             :<|> "getCurrentMerchant"
               :> DashboardAuth 'DASHBOARD_USER
               :> Get '[JSON] DPerson.MerchantAccessRes
             :<|> DashboardAuth 'DASHBOARD_USER
               :> "changePassword"
               :> ReqBody '[JSON] DPerson.ChangePasswordReq
               :> Post '[JSON] APISuccess
             :<|> "changePasswordAfterExpiry"
               :> ReqBody '[JSON] DPerson.ChangePasswordAfterExpiryReq
               :> Post '[JSON] APISuccess
             :<|> "roles"
               :> "list"
               :> DashboardAuth 'DASHBOARD_USER
               :> QueryParam "searchString" Text
               :> QueryParam "limit" Integer
               :> QueryParam "offset" Integer
               :> Get '[JSON] DRoles.ListRoleRes
         )
    :<|> "release"
      :> ( DashboardAuth 'DASHBOARD_RELEASE_ADMIN
             :> ReqBody '[JSON] DPerson.ReleaseRegisterReq
             :> Post '[JSON] DPerson.ReleaseRegisterRes
             :<|> "getProductSpecInfo" -- :> DashboardAuth 'DASHBOARD_ADMIN
               :> QueryParam "releaseId" Text
               :> Get '[JSON] DPerson.GetProductSpecInfoResp
         )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  ( listPerson
      :<|> ptList
      :<|> assignRole
      :<|> assignMerchantAccess -- TODO : Deprecated, Remove after successful deployment
      :<|> assignMerchantCityAccess
      :<|> resetMerchantAccess
      :<|> resetMerchantCityAccess
      :<|> createPerson
      :<|> deletePerson
      :<|> changeEnabledStatus
      :<|> changeEmailByAdmin
      :<|> changePasswordByAdmin
      :<|> changeMobileByAdmin
  )
    :<|> ( profile
             :<|> updateProfile
             :<|> getCurrentMerchant
             :<|> changePassword
             :<|> changePasswordAfterExpiry
             :<|> listRolesForUser
         )
    :<|> ( registerRelease
             :<|> getProductSpecInfo
         )

listPerson :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe (Id DP.Person) -> FlowHandlerR r DPerson.ListPersonRes
listPerson tokenInfo mbSearchString mbLimit mbPersonId =
  withDashboardDbFlowHandlerAPI . DPerson.listPerson tokenInfo mbSearchString mbLimit mbPersonId

ptList :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandlerR r DPerson.ListPTEmployeeRes
ptList tokenInfo mbSearchString mbRoleName mbEntityShortId mbLimit =
  withDashboardDbFlowHandlerAPI . DPerson.ptList tokenInfo mbSearchString mbRoleName mbEntityShortId mbLimit

createPerson :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DPerson.CreatePersonReq -> FlowHandlerR r DPerson.CreatePersonRes
createPerson tokenInfo = withDashboardDbFlowHandlerAPI . DPerson.createPerson tokenInfo

assignRole :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> Id DRole.Role -> FlowHandlerR r APISuccess
assignRole tokenInfo personId =
  withDashboardDbFlowHandlerAPI . DPerson.assignRole tokenInfo personId

assignMerchantAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandlerR r APISuccess
assignMerchantAccess tokenInfo personId req = do
  city <- withDashboardDbFlowHandlerAPI $ QMerchant.findByShortId req.merchantId >>= fmap (.defaultOperatingCity) . fromMaybeM (MerchantNotFound req.merchantId.getShortId)
  let req' = DPerson.MerchantCityAccessReq {merchantId = req.merchantId, operatingCity = city}
  withDashboardDbFlowHandlerAPI $ DPerson.assignMerchantCityAccess tokenInfo personId req'

assignMerchantCityAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.MerchantCityAccessReq -> FlowHandlerR r APISuccess
assignMerchantCityAccess tokenInfo personId =
  withDashboardDbFlowHandlerAPI . DPerson.assignMerchantCityAccess tokenInfo personId

resetMerchantAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandlerR r APISuccess
resetMerchantAccess tokenInfo personId =
  withDashboardDbFlowHandlerAPI . DPerson.resetMerchantAccess tokenInfo personId

resetMerchantCityAccess :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.MerchantCityAccessReq -> FlowHandlerR r APISuccess
resetMerchantCityAccess tokenInfo personId =
  withDashboardDbFlowHandlerAPI . DPerson.resetMerchantCityAccess tokenInfo personId

profile :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DP.PersonAPIEntity
profile =
  withDashboardDbFlowHandlerAPI . DPerson.profile

updateProfile :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DPerson.UpdateProfileReq -> FlowHandlerR r APISuccess
updateProfile tokenInfo =
  withDashboardDbFlowHandlerAPI . DPerson.updateProfile tokenInfo

getCurrentMerchant :: DashboardLoginFlow (FlowR r) r => TokenInfo -> FlowHandlerR r DPerson.MerchantAccessRes
getCurrentMerchant =
  withDashboardDbFlowHandlerAPI . DPerson.getCurrentMerchant

changePassword :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DPerson.ChangePasswordReq -> FlowHandlerR r APISuccess
changePassword req =
  withDashboardDbFlowHandlerAPI . DPerson.changePassword req

changePasswordAfterExpiry :: DashboardLoginFlow (FlowR r) r => DPerson.ChangePasswordAfterExpiryReq -> FlowHandlerR r APISuccess
changePasswordAfterExpiry req =
  withDashboardDbFlowHandlerAPI $ DPerson.changePasswordAfterExpiry req

listRolesForUser :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandlerR r DRoles.ListRoleRes
listRolesForUser tokenInfo mbSearchString mbLimit mbOffset =
  withDashboardDbFlowHandlerAPI (DRoles.listRolesV2 tokenInfo mbSearchString mbLimit mbOffset)

changeEmailByAdmin :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.ChangeEmailByAdminReq -> FlowHandlerR r APISuccess
changeEmailByAdmin tokenInfo personId req =
  withDashboardDbFlowHandlerAPI $ DPerson.changeEmailByAdmin tokenInfo personId req

changePasswordByAdmin :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.ChangePasswordByAdminReq -> FlowHandlerR r APISuccess
changePasswordByAdmin tokenInfo personId req =
  withDashboardDbFlowHandlerAPI $ DPerson.changePasswordByAdmin tokenInfo personId req

changeMobileByAdmin :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.ChangeMobileNumberByAdminReq -> FlowHandlerR r APISuccess
changeMobileByAdmin tokenInfo personId req =
  withDashboardDbFlowHandlerAPI $ DPerson.changeMobileNumberByAdmin tokenInfo personId req

registerRelease :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DPerson.ReleaseRegisterReq -> FlowHandlerR r DPerson.ReleaseRegisterRes
registerRelease tokenInfo = withDashboardDbFlowHandlerAPI . DPerson.registerRelease tokenInfo

getProductSpecInfo :: DashboardLoginFlow (FlowR r) r => Maybe Text -> FlowHandlerR r DPerson.GetProductSpecInfoResp
getProductSpecInfo releaseId =
  withDashboardDbFlowHandlerAPI $ DPerson.getProductSpecInfo releaseId

deletePerson :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> Maybe Text -> FlowHandlerR r APISuccess
deletePerson tokenInfo personId mbDeleteReason =
  withDashboardDbFlowHandlerAPI $ DPerson.deletePerson tokenInfo personId mbDeleteReason

changeEnabledStatus :: DashboardLoginFlow (FlowR r) r => TokenInfo -> Id DP.Person -> DPerson.ChangeEnabledStatusReq -> FlowHandlerR r APISuccess
changeEnabledStatus tokenInfo personId req =
  withDashboardDbFlowHandlerAPI $ DPerson.changeEnabledStatus tokenInfo personId req
