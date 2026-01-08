{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Registration
  ( DRegistration.AuthReq (..),
    DRegistration.AuthRes (..),
    DRegistration.ResendAuthRes,
    DRegistration.AuthVerifyReq (..),
    DRegistration.AuthVerifyRes (..),
    DRegistration.MarketEventReq (..),
    API,
    handler,
  )
where

import qualified Domain.Action.UI.Registration as DRegistration
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.SignatureAuth

type API =
  "auth"
    :> ( ReqBody '[JSON] DRegistration.AuthReq
           :> Header "x-bundle-version" Version
           :> Header "x-client-version" Version
           :> Header "x-config-version" Version
           :> Header "x-react-bundle-version" Text
           :> Header "x-package" Text
           :> Header "x-device" Text
           :> Header "x-sender-hash" Text
           :> Post '[JSON] DRegistration.AuthRes
           :<|> "signature"
             :> SignatureAuth DRegistration.AuthReq "x-sdk-authorization"
             :> Header "x-bundle-version" Version
             :> Header "x-client-version" Version
             :> Header "x-config-version" Version
             :> Header "x-react-bundle-version" Text
             :> Header "x-package" Text
             :> Header "x-device" Text
             :> Post '[JSON] DRegistration.AuthRes
           :<|> Capture "authId" (Id SR.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] DRegistration.AuthVerifyReq
             :> Post '[JSON] DRegistration.AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SR.RegistrationToken)
             :> Header "x-sender-hash" Text
             :> "resend"
             :> Post '[JSON] DRegistration.ResendAuthRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
           :<|> "marketing"
             :> "events"
             :> ReqBody '[JSON] DRegistration.MarketEventReq
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  auth
    :<|> signatureAuth
    :<|> verify
    :<|> resend
    :<|> logout
    :<|> marketingEvents

auth :: DRegistration.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler DRegistration.AuthRes
auth req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash = withFlowHandlerAPI $ DRegistration.auth False req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash

signatureAuth :: SignatureAuthResult DRegistration.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler DRegistration.AuthRes
signatureAuth (SignatureAuthResult req) mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice =
  withFlowHandlerAPI $ DRegistration.signatureAuth req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice mbClientId

verify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> FlowHandler DRegistration.AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DRegistration.verify tokenId

resend :: Id SR.RegistrationToken -> Maybe Text -> FlowHandler DRegistration.ResendAuthRes
resend tokenId mbSenderHash = withFlowHandlerAPI $ DRegistration.resend tokenId mbSenderHash

logout :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
logout = withFlowHandlerAPI . DRegistration.logout

marketingEvents :: DRegistration.MarketEventReq -> FlowHandler APISuccess
marketingEvents = withFlowHandlerAPI . DRegistration.marketingEventsPreLogin
