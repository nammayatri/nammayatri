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
    DProfile.UpdateProfileResp,
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
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

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
           :> Header "x-package" Text
           :> Get '[JSON] DProfile.ProfileRes
           :<|> TokenAuth
             :> ReqBody '[JSON] DProfile.UpdateProfileReq
             :> Header "x-bundle-version" Version
             :> Header "x-rn-version" Text
             :> Header "x-client-version" Version
             :> Header "x-config-version" Version
             :> Header "x-device" Text
             :> Header "x-package" Text
             :> Post '[JSON] APISuccess.APISuccess
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
    :<|> marketingEvents
    :<|> (triggerOTP :<|> verifyOTP)

getPersonDetails :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> FlowHandler DProfile.ProfileRes
getPersonDetails (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId = withFlowHandlerAPIPersonId personId $ getPersonDetails' (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId

getPersonDetails' :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Flow DProfile.ProfileRes
getPersonDetails' (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId = DProfile.getPersonDetails (personId, merchantId) toss tenant context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId

updatePerson :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileReq -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> FlowHandler APISuccess.APISuccess
updatePerson (personId, merchantId) req mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId = withFlowHandlerAPIPersonId personId $ updatePerson' (personId, merchantId) req mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId

updatePerson' :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.UpdateProfileReq -> Maybe Version -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Flow APISuccess.APISuccess
updatePerson' (personId, merchantId) req mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId = withPersonIdLogTag personId $ DProfile.updatePerson personId merchantId req mbRnVersion mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId

marketingEvents' :: DProfile.MarketEventReq -> Flow APISuccess.APISuccess
marketingEvents' req = DProfile.marketingEvents req

marketingEvents :: DProfile.MarketEventReq -> FlowHandler APISuccess.APISuccess
marketingEvents req = withFlowHandlerAPI $ marketingEvents' req

triggerOTP' :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.TriggerUpdateAuthOTPReq -> Flow APISuccess.APISuccess
triggerOTP' (personId, merchantId) = DProfile.triggerUpdateAuthDataOtp (personId, merchantId)

triggerOTP :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.TriggerUpdateAuthOTPReq -> FlowHandler APISuccess.APISuccess
triggerOTP (personId, merchantId) req = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ triggerOTP' (personId, merchantId) req

verifyOTP' :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.VerifyUpdateAuthOTPReq -> Flow APISuccess.APISuccess
verifyOTP' (personId, merchantId) = DProfile.verifyUpdateAuthDataOtp (personId, merchantId)

verifyOTP :: (Id Person.Person, Id Merchant.Merchant) -> DProfile.VerifyUpdateAuthOTPReq -> FlowHandler APISuccess.APISuccess
verifyOTP (personId, merchantId) req = withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $ verifyOTP' (personId, merchantId) req
