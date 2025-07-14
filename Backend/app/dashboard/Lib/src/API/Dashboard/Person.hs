{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Person where

import qualified "dashboard-helper-api" Dashboard.Common.Driver as Common
import qualified Domain.Action.Dashboard.Person as DPerson
import qualified Domain.Action.Dashboard.Transaction as DTransaction
import Domain.Types.AccessMatrix
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.Transaction as DT
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, withFlowHandlerAPI')
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import Tools.Auth
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
             :<|> "getCurrentMerchant"
               :> DashboardAuth 'DASHBOARD_USER
               :> Get '[JSON] DPerson.MerchantAccessRes
             :<|> DashboardAuth 'DASHBOARD_USER
               :> "changePassword"
               :> ReqBody '[JSON] DPerson.ChangePasswordReq
               :> Post '[JSON] APISuccess
             :<|> "getAccessMatrix"
               :> DashboardAuth 'DASHBOARD_USER
               :> Get '[JSON] DMatrix.AccessMatrixRowAPIEntity
             :<|> "changePasswordAfterExpiry"
               :> ReqBody '[JSON] DPerson.ChangePasswordAfterExpiryReq
               :> Post '[JSON] APISuccess
         )
    :<|> "release"
      :> ( DashboardAuth 'DASHBOARD_RELEASE_ADMIN
             :> ReqBody '[JSON] DPerson.ReleaseRegisterReq
             :> Post '[JSON] DPerson.ReleaseRegisterRes
             :<|> "getProductSpecInfo" -- :> DashboardAuth 'DASHBOARD_ADMIN
               :> QueryParam "releaseId" Text
               :> Get '[JSON] DPerson.GetProductSpecInfoResp
         )
    :<|> "listTransactions"
      :> DashboardAuth 'DASHBOARD_USER
      :> QueryParam "searchString" Text
      :> QueryParam "limit" Integer
      :> QueryParam "offset" Integer
      :> QueryParam "requestorId" (Id DP.Person)
      :> QueryParam "driverId" (Id Common.Driver)
      :> QueryParam "rideId" (Id Common.Ride)
      :> QueryParam "endpoint" (DT.Endpoint)
      :> Get '[JSON] DTransaction.ListTransactionRes

handler :: BeamFlow' => FlowServer API
handler =
  ( listPerson
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
             :<|> getCurrentMerchant
             :<|> changePassword
             :<|> getAccessMatrix
             :<|> changePasswordAfterExpiry
         )
    :<|> ( registerRelease
             :<|> getProductSpecInfo
         )
    :<|> listTransactions

listPerson :: BeamFlow' => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe (Id DP.Person) -> FlowHandler DPerson.ListPersonRes
listPerson tokenInfo mbSearchString mbLimit mbPersonId =
  withFlowHandlerAPI' . DPerson.listPerson tokenInfo mbSearchString mbLimit mbPersonId

createPerson :: BeamFlow' => TokenInfo -> DPerson.CreatePersonReq -> FlowHandler DPerson.CreatePersonRes
createPerson tokenInfo = withFlowHandlerAPI' . DPerson.createPerson tokenInfo

assignRole :: BeamFlow' => TokenInfo -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole tokenInfo personId =
  withFlowHandlerAPI' . DPerson.assignRole tokenInfo personId

assignMerchantAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
assignMerchantAccess tokenInfo personId req = do
  city <- withFlowHandlerAPI' $ QMerchant.findByShortId req.merchantId >>= fmap (.defaultOperatingCity) . fromMaybeM (MerchantNotFound req.merchantId.getShortId)
  let req' = DPerson.MerchantCityAccessReq {merchantId = req.merchantId, operatingCity = city}
  withFlowHandlerAPI' $ DPerson.assignMerchantCityAccess tokenInfo personId req'

assignMerchantCityAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.MerchantCityAccessReq -> FlowHandler APISuccess
assignMerchantCityAccess tokenInfo personId =
  withFlowHandlerAPI' . DPerson.assignMerchantCityAccess tokenInfo personId

resetMerchantAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
resetMerchantAccess tokenInfo personId =
  withFlowHandlerAPI' . DPerson.resetMerchantAccess tokenInfo personId

resetMerchantCityAccess :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.MerchantCityAccessReq -> FlowHandler APISuccess
resetMerchantCityAccess tokenInfo personId =
  withFlowHandlerAPI' . DPerson.resetMerchantCityAccess tokenInfo personId

profile :: BeamFlow' => TokenInfo -> FlowHandler DP.PersonAPIEntity
profile =
  withFlowHandlerAPI' . DPerson.profile

getCurrentMerchant :: BeamFlow' => TokenInfo -> FlowHandler DPerson.MerchantAccessRes
getCurrentMerchant =
  withFlowHandlerAPI' . DPerson.getCurrentMerchant

changePassword :: BeamFlow' => TokenInfo -> DPerson.ChangePasswordReq -> FlowHandler APISuccess
changePassword req =
  withFlowHandlerAPI' . DPerson.changePassword req

changePasswordAfterExpiry :: BeamFlow' => DPerson.ChangePasswordAfterExpiryReq -> FlowHandler APISuccess
changePasswordAfterExpiry req =
  withFlowHandlerAPI' $ DPerson.changePasswordAfterExpiry req

getAccessMatrix :: BeamFlow' => TokenInfo -> FlowHandler AccessMatrixRowAPIEntity
getAccessMatrix =
  withFlowHandlerAPI' . DPerson.getAccessMatrix

changeEmailByAdmin :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.ChangeEmailByAdminReq -> FlowHandler APISuccess
changeEmailByAdmin tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changeEmailByAdmin tokenInfo personId req

changePasswordByAdmin :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.ChangePasswordByAdminReq -> FlowHandler APISuccess
changePasswordByAdmin tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changePasswordByAdmin tokenInfo personId req

changeMobileByAdmin :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.ChangeMobileNumberByAdminReq -> FlowHandler APISuccess
changeMobileByAdmin tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changeMobileNumberByAdmin tokenInfo personId req

registerRelease :: BeamFlow' => TokenInfo -> DPerson.ReleaseRegisterReq -> FlowHandler DPerson.ReleaseRegisterRes
registerRelease tokenInfo = withFlowHandlerAPI' . DPerson.registerRelease tokenInfo

getProductSpecInfo :: BeamFlow' => Maybe Text -> FlowHandler DPerson.GetProductSpecInfoResp
getProductSpecInfo releaseId =
  withFlowHandlerAPI' $ DPerson.getProductSpecInfo releaseId

deletePerson :: BeamFlow' => TokenInfo -> Id DP.Person -> FlowHandler APISuccess
deletePerson tokenInfo personId =
  withFlowHandlerAPI' $ DPerson.deletePerson tokenInfo personId

changeEnabledStatus :: BeamFlow' => TokenInfo -> Id DP.Person -> DPerson.ChangeEnabledStatusReq -> FlowHandler APISuccess
changeEnabledStatus tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changeEnabledStatus tokenInfo personId req

listTransactions :: BeamFlow' => TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe (Id DP.Person) -> Maybe (Id Common.Driver) -> Maybe (Id Common.Ride) -> Maybe DT.Endpoint -> FlowHandler DTransaction.ListTransactionRes
listTransactions tokenInfo mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint =
  withFlowHandlerAPI' $ DTransaction.listTransactions tokenInfo mbSearchString mbLimit mbOffset mbRequestorId mbDriverId mbRideId mbEndpoint
