{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Profile
  ( DProfile.ProfileRes,
    DProfile.UpdateProfileReq (..),
    DProfile.UpdateEmergencySettingsReq (..),
    DProfile.UpdateEmergencySettingsResp,
    DProfile.UpdateProfileResp,
    DProfile.UpdateProfileDefaultEmergencyNumbersReq (..),
    DProfile.PersonDefaultEmergencyNumber (..),
    DProfile.UpdateProfileDefaultEmergencyNumbersResp,
    DProfile.GetProfileDefaultEmergencyNumbersResp (..),
    DProfile.TriggerUpdateAuthOTPReq (..),
    DProfile.VerifyUpdateAuthOTPReq (..),
    API,
    getPersonDetails',
    updatePerson',
    handler,
    triggerOTP',
    verifyOTP',
  )
where

import qualified Domain.Action.UI.Profile as DProfile
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "profile"
    :> ( TokenAuth
           :> QueryParam "toss" Int
           :> QueryParam "tenant" Text
           :> QueryParam "context" Text
           :> QueryParam "includeProfileImage" Bool
           :> Header "x-bundle-version" Version
           :> Header "x-rn-version" Text
           :> Header "x-client-version" Version
           :> Header "x-config-version" Version
           :> Header "x-device" Text
           :> Get '[JSON] DProfile.ProfileRes
           :<|> TokenAuth
             :> ReqBody '[JSON] DProfile.UpdateProfileReq
             :> Header "x-bundle-version" Version
             :> Header "x-rn-version" Text
             :> Header "x-client-version" Version
             :> Header "x-config-version" Version
             :> Header "x-device" Text
             :> Post '[JSON] APISuccess.APISuccess
           :<|> "updateEmergencySettings"
             :> ( TokenAuth
                    :> ReqBody '[JSON] DProfile.UpdateEmergencySettingsReq
                    :> Put '[JSON] APISuccess.APISuccess
                )
           :<|> "getEmergencySettings"
             :> TokenAuth
             :> Get '[JSON] DProfile.EmergencySettingsRes
           :<|> "defaultEmergencyNumbers"
             :> ( TokenAuth
                    :> ReqBody '[JSON] DProfile.UpdateProfileDefaultEmergencyNumbersReq
                    :> Post '[JSON] DProfile.UpdateProfileDefaultEmergencyNumbersResp
                      :<|> TokenAuth
                    :> Get '[JSON] DProfile.GetProfileDefaultEmergencyNumbersResp
                )
           :<|> "marketing"
             :> ( "events"
                    :> ReqBody '[JSON] DProfile.MarketEventReq
                    :> Post '[JSON] APISuccess.APISuccess
                )
           :<|> "updateAuthData"
             :> ( "triggerOTP"
                    :> TokenAuth
                    :> ReqBody '[JSON] DProfile.TriggerUpdateAuthOTPReq
                    :> Post '[JSON] APISuccess.APISuccess
                    :<|> "verifyOTP"
                      :> TokenAuth
                      :> ReqBody '[JSON] DProfile.VerifyUpdateAuthOTPReq
                      :> Post '[JSON] APISuccess.APISuccess
                )
       )

handler :: FlowServer API
handler =
  getPersonDetails
    :<|> updatePerson
    :<|> updateEmergencySettings
    :<|> getEmergencySettings
    :<|> (updateDefaultEmergencyNumbers :<|> getDefaultEmergencyNumbers)
    :<|> marketingEvents
    :<|> (triggerOTP :<|> verifyOTP)

getPersonDetails :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> FlowHandler DProfile.ProfileRes
getPersonDetails (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion = withFlowHandlerAPI . getPersonDetails' (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion

getPersonDetails' :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> Flow DProfile.ProfileRes
getPersonDetails' (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion = DProfile.getPersonDetails (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion

updatePerson :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileReq -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> FlowHandler APISuccess.APISuccess
updatePerson (personId, merchantId) req mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion = withFlowHandlerAPI . updatePerson' (personId, merchantId) req mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion

updatePerson' :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileReq -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> Flow APISuccess.APISuccess
updatePerson' (personId, merchantId) req mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion = withPersonIdLogTag personId . DProfile.updatePerson personId merchantId req mbRnVersion mbBundleVersion mbClientVersion mbClientConfigVersion

updateDefaultEmergencyNumbers :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileDefaultEmergencyNumbersReq -> FlowHandler DProfile.UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers (personId, merchantId) = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updateDefaultEmergencyNumbers personId merchantId

getDefaultEmergencyNumbers :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DProfile.GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers = withFlowHandlerAPI . DProfile.getDefaultEmergencyNumbers

updateEmergencySettings :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateEmergencySettingsReq -> FlowHandler DProfile.UpdateEmergencySettingsResp
updateEmergencySettings (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId . DProfile.updateEmergencySettings personId

getEmergencySettings :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DProfile.EmergencySettingsRes
getEmergencySettings (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId $ DProfile.getEmergencySettings personId

marketingEvents' :: DProfile.MarketEventReq -> Flow APISuccess.APISuccess
marketingEvents' req = DProfile.marketingEvents req

marketingEvents :: DProfile.MarketEventReq -> FlowHandler APISuccess.APISuccess
marketingEvents req = withFlowHandlerAPI $ marketingEvents' req

triggerOTP' :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.TriggerUpdateAuthOTPReq -> Flow APISuccess.APISuccess
triggerOTP' (personId, merchantId) = DProfile.triggerUpdateAuthDataOtp (personId, merchantId)

triggerOTP :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.TriggerUpdateAuthOTPReq -> FlowHandler APISuccess.APISuccess
triggerOTP (personId, merchantId) req = withFlowHandlerAPI $ triggerOTP' (personId, merchantId) req

verifyOTP' :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.VerifyUpdateAuthOTPReq -> Flow APISuccess.APISuccess
verifyOTP' (personId, merchantId) = DProfile.verifyUpdateAuthDataOtp (personId, merchantId)

verifyOTP :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.VerifyUpdateAuthOTPReq -> FlowHandler APISuccess.APISuccess
verifyOTP (personId, merchantId) req = withFlowHandlerAPI $ verifyOTP' (personId, merchantId) req
