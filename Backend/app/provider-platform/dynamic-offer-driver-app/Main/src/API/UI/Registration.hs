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

import AppAttestation.API (WithAttestationHeaders, mkAttestationHeaders)
import qualified AppAttestation.Challenge as Attestation
import qualified AppAttestation.KeyRegistry as Attestation
import AppAttestation.Types (AttestationHeaders)
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
import qualified TempAppCode.Types as Fields
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
           :> Header "x-forwarded-for" Text
           :> WithAttestationHeaders (Post '[JSON] DRegistration.AuthRes)
           :<|> "attestation"
             :> ( "challenge"
                    :> Header "x-forwarded-for" Text
                    :> Get '[JSON] Attestation.Challenge
                    :<|> "ios"
                      :> "key"
                      :> Header "x-forwarded-for" Text
                      :> ReqBody '[JSON] Attestation.RegisterKeyReq
                      :> Post '[JSON] Attestation.RegisterKeyRes
                )
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
             :> Header "x-forwarded-for" Text
             :> Post '[JSON] DRegistration.AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SR.RegistrationToken)
             :> Header "x-sender-hash" Text
             :> Header "x-forwarded-for" Text
             :> WithAttestationHeaders ("resend" :> Post '[JSON] DRegistration.ResendAuthRes)
           :<|> "generate-temp-app-code"
             :> TokenAuth
             :> Post '[JSON] DRegistration.TempCodeRes
           :<|> "get-token"
             :> ReqBody '[JSON] DRegistration.GetTokenReq
             :> Post '[JSON] DRegistration.AuthRes
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
    :<|> (attestationChallenge :<|> registerAppAttestKey)
    :<|> signatureAuth
    :<|> verify
    :<|> resend
    :<|> generateTempAppCode
    :<|> getToken
    :<|> logout
    :<|> marketingEvents

type AttestationHeadersHandler res = Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler res

withAttestationHeaders :: (AttestationHeaders -> FlowHandler res) -> AttestationHeadersHandler res
withAttestationHeaders continueWith platform challengeId token keyId assertion clientError captchaToken =
  continueWith $ mkAttestationHeaders platform challengeId token keyId assertion clientError captchaToken

auth :: DRegistration.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AttestationHeadersHandler DRegistration.AuthRes
auth req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash mbXForwardedFor =
  withAttestationHeaders $
    withFlowHandlerAPI . DRegistration.auth False req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice mbSenderHash mbXForwardedFor . Just

attestationChallenge :: Maybe Text -> FlowHandler Attestation.Challenge
attestationChallenge mbXForwardedFor = withFlowHandlerAPI $ DRegistration.extractClientIP mbXForwardedFor >>= Attestation.createChallenge

registerAppAttestKey :: Maybe Text -> Attestation.RegisterKeyReq -> FlowHandler Attestation.RegisterKeyRes
registerAppAttestKey mbXForwardedFor = withFlowHandlerAPI . DRegistration.registerAppAttestKey mbXForwardedFor

signatureAuth :: SignatureAuthResult DRegistration.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler DRegistration.AuthRes
signatureAuth (SignatureAuthResult req) mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbClientId mbDevice =
  withFlowHandlerAPI $ DRegistration.signatureAuth req mbBundleVersion mbClientVersion mbClientConfigVersion mbReactBundleVersion mbDevice mbClientId

verify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> Maybe Text -> FlowHandler DRegistration.AuthVerifyRes
verify tokenId req mbXForwardedFor = withFlowHandlerAPI $ DRegistration.verify tokenId req mbXForwardedFor

resend :: Id SR.RegistrationToken -> Maybe Text -> Maybe Text -> AttestationHeadersHandler DRegistration.ResendAuthRes
resend tokenId mbSenderHash mbXForwardedFor =
  withAttestationHeaders $
    withFlowHandlerAPI . DRegistration.resend tokenId mbSenderHash mbXForwardedFor . Just

generateTempAppCode :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DRegistration.TempCodeRes
generateTempAppCode (personId, _, _) =
  withFlowHandlerAPI $
    DRegistration.TempCodeRes . (.code)
      <$> DRegistration.generateTempAppCode DRegistration.driverTempAppCodeCfg personId

getToken :: DRegistration.GetTokenReq -> FlowHandler DRegistration.AuthRes
getToken = withFlowHandlerAPI . DRegistration.getToken

logout :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
logout = withFlowHandlerAPI . DRegistration.logout

marketingEvents :: DRegistration.MarketEventReq -> FlowHandler APISuccess
marketingEvents = withFlowHandlerAPI . DRegistration.marketingEventsPreLogin
