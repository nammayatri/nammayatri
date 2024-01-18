{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Person where

import "lib-dashboard" API.Dashboard.Person (API)
import qualified "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import "lib-dashboard" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.Role as DRole
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, withFlowHandlerAPI')
import Servant hiding (Unauthorized, throwError)
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Error

-- Note : Type of API is defined in lib-dashboard/API/Dashboard/Person.hs

handler :: FlowServer API
handler =
  ( listPerson
      :<|> assignRole
      :<|> assignMerchantAccess -- TODO : Deprecated, Remove after successful deployment
      :<|> assignMerchantCityAccess
      :<|> resetMerchantAccess
      :<|> resetMerchantCityAccess
      :<|> createPerson
      :<|> changeEmailByAdmin
      :<|> changePasswordByAdmin
      :<|> changeMobileByAdmin
  )
    :<|> ( profile
             :<|> getCurrentMerchant
             :<|> changePassword
             :<|> getAccessMatrix
         )
    :<|> registerRelease
    :<|> getProductSpecInfo

listPerson :: TokenInfo -> Maybe Text -> Maybe Integer -> Maybe Integer -> Maybe (Id DP.Person) -> FlowHandler DPerson.ListPersonRes
listPerson tokenInfo mbSearchString mbLimit mbPersonId =
  withFlowHandlerAPI' . DPerson.listPerson tokenInfo mbSearchString mbLimit mbPersonId

createPerson :: TokenInfo -> DPerson.CreatePersonReq -> FlowHandler DPerson.CreatePersonRes
createPerson tokenInfo = withFlowHandlerAPI' . DPerson.createPerson tokenInfo

assignRole :: TokenInfo -> Id DP.Person -> Id DRole.Role -> FlowHandler APISuccess
assignRole tokenInfo personId =
  withFlowHandlerAPI' . DPerson.assignRole tokenInfo personId

assignMerchantAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
assignMerchantAccess tokenInfo personId req = do
  city <- withFlowHandlerAPI' $ QMerchant.findByShortId req.merchantId >>= fmap (.defaultOperatingCity) . fromMaybeM (MerchantNotFound req.merchantId.getShortId)
  let req' = DPerson.MerchantCityAccessReq {merchantId = req.merchantId, operatingCity = city}
  withFlowHandlerAPI' $ DPerson.assignMerchantCityAccess tokenInfo personId req'

assignMerchantCityAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantCityAccessReq -> FlowHandler APISuccess
assignMerchantCityAccess tokenInfo personId =
  withFlowHandlerAPI' . DPerson.assignMerchantCityAccess tokenInfo personId

resetMerchantAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantAccessReq -> FlowHandler APISuccess
resetMerchantAccess tokenInfo personId =
  withFlowHandlerAPI' . DPerson.resetMerchantAccess tokenInfo personId

resetMerchantCityAccess :: TokenInfo -> Id DP.Person -> DPerson.MerchantCityAccessReq -> FlowHandler APISuccess
resetMerchantCityAccess tokenInfo personId =
  withFlowHandlerAPI' . DPerson.resetMerchantCityAccess tokenInfo personId

profile :: TokenInfo -> FlowHandler DP.PersonAPIEntity
profile =
  withFlowHandlerAPI' . DPerson.profile

getCurrentMerchant :: TokenInfo -> FlowHandler DPerson.MerchantAccessRes
getCurrentMerchant =
  withFlowHandlerAPI' . DPerson.getCurrentMerchant

changePassword :: TokenInfo -> DPerson.ChangePasswordReq -> FlowHandler APISuccess
changePassword req =
  withFlowHandlerAPI' . DPerson.changePassword req

getAccessMatrix :: TokenInfo -> FlowHandler AccessMatrixRowAPIEntity
getAccessMatrix =
  withFlowHandlerAPI' . DPerson.getAccessMatrix

changeEmailByAdmin :: TokenInfo -> Id DP.Person -> DPerson.ChangeEmailByAdminReq -> FlowHandler APISuccess
changeEmailByAdmin tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changeEmailByAdmin tokenInfo personId req

changePasswordByAdmin :: TokenInfo -> Id DP.Person -> DPerson.ChangePasswordByAdminReq -> FlowHandler APISuccess
changePasswordByAdmin tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changePasswordByAdmin tokenInfo personId req

changeMobileByAdmin :: TokenInfo -> Id DP.Person -> DPerson.ChangeMobileNumberByAdminReq -> FlowHandler APISuccess
changeMobileByAdmin tokenInfo personId req =
  withFlowHandlerAPI' $ DPerson.changeMobileNumberByAdmin tokenInfo personId req

registerRelease :: TokenInfo -> DPerson.ReleaseRegisterReq -> FlowHandler DPerson.ReleaseRegisterRes
registerRelease tokenInfo = withFlowHandlerAPI' . DPerson.registerRelease tokenInfo

getProductSpecInfo :: Maybe Text -> FlowHandler DPerson.GetProductSpecInfoResp
getProductSpecInfo releaseId =
  withFlowHandlerAPI' $ DPerson.getProductSpecInfo releaseId
