{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Registration where

import API.UI.Registration hiding (API)
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.RegistrationToken as SRT
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

-- Customer Registration Flow ------
data RegistrationEndPoint
  = RegistrationAuthEndPoint
  | RegistrationVerifyEndPoint
  | RegistrationResendEndPoint
  | RegistrationLogoutEndPoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "RegistrationEndPoint"

type API =
  "registration"
    :> ( CustomerAuthAPI
           :<|> CustomerVerify
           :<|> CustomerResend
           :<|> CustomerLogout
       )

type CustomerAuthAPI =
  "auth"
    :> ReqBody '[JSON] CustomerAuthReq
    :> Post '[JSON] DRegistration.AuthRes

type CustomerVerify =
  Capture "authId" (Id SRT.RegistrationToken)
    :> "verify"
    :> ReqBody '[JSON] DRegistration.AuthVerifyReq
    :> Post '[JSON] DRegistration.AuthVerifyRes

type CustomerResend =
  "otp"
    :> Capture "authId" (Id SRT.RegistrationToken)
    :> "resend"
    :> Post '[JSON] DRegistration.ResendAuthRes

type CustomerLogout =
  "logout"
    :> Capture "customerId" (Id SP.Person)
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  callAuth merchantId
    :<|> callVerify
    :<|> callResend
    :<|> callLogout

callAuth :: ShortId DM.Merchant -> CustomerAuthReq -> FlowHandler DRegistration.AuthRes
callAuth merchantShortId req = do
  let authReq = buildAuthReq merchantShortId req
  withFlowHandlerAPI $ DRegistration.auth authReq Nothing Nothing

callVerify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> FlowHandler DRegistration.AuthVerifyRes
callVerify tokenId req = withFlowHandlerAPI $ DRegistration.verify tokenId req

callResend :: Id SR.RegistrationToken -> FlowHandler DRegistration.ResendAuthRes
callResend = withFlowHandlerAPI . DRegistration.resend

callLogout :: Id SP.Person -> FlowHandler APISuccess
callLogout personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DRegistration.logout personId

buildAuthReq :: ShortId DM.Merchant -> CustomerAuthReq -> DRegistration.AuthReq
buildAuthReq merchantShortId req =
  DRegistration.AuthReq
    { mobileNumber = req.mobileNumber,
      mobileCountryCode = req.mobileCountryCode,
      merchantId = merchantShortId,
      deviceToken = Nothing,
      notificationToken = Nothing,
      whatsappNotificationEnroll = Nothing,
      firstName = Nothing,
      middleName = Nothing,
      lastName = Nothing,
      email = Nothing,
      language = Nothing,
      gender = Nothing,
      otpChannel = req.otpChannel,
      registrationLat = Nothing,
      registrationLon = Nothing
    }

data CustomerAuthReq = CutomerAuthReq
  { mobileNumber :: Text,
    mobileCountryCode :: Text,
    otpChannel :: Maybe DRegistration.OTPChannel
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
