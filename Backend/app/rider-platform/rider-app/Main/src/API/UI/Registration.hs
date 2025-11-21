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
    DRegistration.PasswordAuthReq (..),
    DRegistration.GetTokenReq (..),
    DRegistration.TempCodeRes (..),
    DRegistration.CustomerSignatureRes (..),
    DRegistration.SendBusinessEmailVerificationReq (..),
    DRegistration.SendBusinessEmailVerificationRes (..),
    DRegistration.VerifyBusinessEmailReq (..),
    DRegistration.VerifyBusinessEmailRes (..),
    HTMLResponse (..),
    API,
    handler,
  )
where

import qualified Data.ByteString.Lazy as BS
import Data.OpenApi (NamedSchema (..), ToSchema (..), binarySchema)
import qualified Data.Text.Encoding as TE
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.RegistrationToken as SRT
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.Servant.HTML
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.Person as Person
import Tools.Auth (TokenAuth)
import Tools.Error
import Tools.SignatureAuth (SignatureAuth, SignatureAuthResult (..))
import Tools.SignatureResponseBody (SignedResponse)

-- Newtype wrapper for HTML responses to support OpenAPI schema generation
newtype HTMLResponse = HTMLResponse BS.ByteString
  deriving (Show)

instance ToSchema HTMLResponse where
  declareNamedSchema _ = pure $ NamedSchema (Just "HTMLResponse") binarySchema

instance MimeRender HTML HTMLResponse where
  mimeRender _ (HTMLResponse bs) = bs

instance MimeUnrender HTML HTMLResponse where
  mimeUnrender _ bs = Right (HTMLResponse bs)

---- Registration Flow ------
type API =
  "auth"
    :> ( ReqBody '[JSON] DRegistration.AuthReq
           :> Header "x-bundle-version" Version
           :> Header "x-client-version" Version
           :> Header "x-config-version" Version
           :> Header "x-rn-version" Text
           :> Header "x-device" Text
           :> Header "x-forwarded-for" Text
           :> Header "x-sender-hash" Text
           :> Post '[JSON] DRegistration.AuthRes
           :<|> "signature"
             :> SignatureAuth DRegistration.AuthReq "x-sdk-authorization"
             :> Header "x-bundle-version" Version
             :> Header "x-client-version" Version
             :> Header "x-config-version" Version
             :> Header "x-rn-version" Text
             :> Header "x-device" Text
             :> Post '[JSON] DRegistration.AuthRes
           :<|> "password"
             :> ReqBody '[JSON] DRegistration.PasswordAuthReq
             :> Post '[JSON] DRegistration.AuthRes
           :<|> "get-token"
             :> ReqBody '[JSON] DRegistration.GetTokenReq
             :> Post '[JSON] DRegistration.AuthRes
           :<|> "business-email"
             :> ( "send-verification"
                    :> TokenAuth
                    :> Post '[JSON] APISuccess
                    :<|> "verify"
                      :> ( ReqBody '[JSON] DRegistration.VerifyBusinessEmailReq
                             :> Post '[JSON] DRegistration.VerifyBusinessEmailRes
                             :<|> TokenAuth
                               :> ReqBody '[JSON] DRegistration.VerifyBusinessEmailReq
                               :> Post '[JSON] DRegistration.VerifyBusinessEmailRes
                         )
                    :<|> "verify-redirect"
                      :> QueryParam' '[Required, Strict] "token" Text
                      :> Get '[HTML] HTMLResponse
                    :<|> "resend"
                      :> TokenAuth
                      :> Post '[JSON] APISuccess
                )
           :<|> Capture "authId" (Id SRT.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] DRegistration.AuthVerifyReq
             :> Post '[JSON] DRegistration.AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SRT.RegistrationToken)
             :> Header "x-sender-hash" Text
             :> "resend"
             :> Post '[JSON] DRegistration.ResendAuthRes
           :<|> "generate-temp-app-code"
             :> TokenAuth
             :> Post '[JSON] DRegistration.TempCodeRes
           :<|> "makeSignature"
             :> TokenAuth
             :> Post '[JSON] (SignedResponse DRegistration.CustomerSignatureRes)
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  auth
    :<|> signatureAuth
    :<|> passwordBasedAuth
    :<|> getToken
    :<|> (sendBusinessEmailVerification :<|> (verifyBusinessEmailWithoutAuth :<|> verifyBusinessEmailWithAuth) :<|> verifyBusinessEmailRedirect :<|> resendBusinessEmailVerification)
    :<|> verify
    :<|> resend
    :<|> generateTempAppCode
    :<|> makeSignature
    :<|> logout

auth :: DRegistration.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowHandler DRegistration.AuthRes
auth req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice mbXForwardedFor mbSenderHash =
  withFlowHandlerAPI $ DRegistration.auth req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice mbXForwardedFor mbSenderHash

signatureAuth :: SignatureAuthResult DRegistration.AuthReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> FlowHandler DRegistration.AuthRes
signatureAuth (SignatureAuthResult req) mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion =
  withFlowHandlerAPI . DRegistration.signatureAuth req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion

verify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> FlowHandler DRegistration.AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DRegistration.verify tokenId

passwordBasedAuth :: DRegistration.PasswordAuthReq -> FlowHandler DRegistration.AuthRes
passwordBasedAuth req = withFlowHandlerAPI $ DRegistration.passwordBasedAuth req

getToken :: DRegistration.GetTokenReq -> FlowHandler DRegistration.AuthRes
getToken req = withFlowHandlerAPI $ DRegistration.getToken req

resend :: Id SR.RegistrationToken -> Maybe Text -> FlowHandler DRegistration.ResendAuthRes
resend tokenId mbSenderHash = withFlowHandlerAPI $ DRegistration.resend tokenId mbSenderHash

generateTempAppCode :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler DRegistration.TempCodeRes
generateTempAppCode (perosnId, _) = withFlowHandlerAPI $ DRegistration.generateTempAppCode perosnId

makeSignature :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler (SignedResponse DRegistration.CustomerSignatureRes)
makeSignature (personId, merchantId) = withFlowHandlerAPI $ DRegistration.makeSignature personId merchantId

logout :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler APISuccess
logout (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId $ DRegistration.logout personId

sendBusinessEmailVerification :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler APISuccess
sendBusinessEmailVerification (personId, merchantId) = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  DRegistration.sendBusinessEmailVerification personId merchantId merchantOperatingCityId

-- Verify business email without auth (for magic link from email)
verifyBusinessEmailWithoutAuth :: DRegistration.VerifyBusinessEmailReq -> FlowHandler DRegistration.VerifyBusinessEmailRes
verifyBusinessEmailWithoutAuth req = withFlowHandlerAPI $ DRegistration.verifyBusinessEmail Nothing req

-- Verify business email with auth (for OTP entered in app)
verifyBusinessEmailWithAuth :: (Id SP.Person, Id Merchant.Merchant) -> DRegistration.VerifyBusinessEmailReq -> FlowHandler DRegistration.VerifyBusinessEmailRes
verifyBusinessEmailWithAuth (personId, _) req = withFlowHandlerAPI $ DRegistration.verifyBusinessEmail (Just personId) req

-- Verify business email via magic link redirect (GET endpoint)
verifyBusinessEmailRedirect :: Text -> FlowHandler HTMLResponse
verifyBusinessEmailRedirect token = withFlowHandlerAPI $ do
  -- Create the verification request with the token
  let verifyReq = DRegistration.VerifyBusinessEmailReq {token = Just token, otp = Nothing}

  -- Call the verification logic
  result <- DRegistration.verifyBusinessEmail Nothing verifyReq

  -- Return HTML response based on success/failure
  if DRegistration.verified result
    then return $ HTMLResponse $ BS.fromStrict $ TE.encodeUtf8 successHtml
    else return $ HTMLResponse $ BS.fromStrict $ TE.encodeUtf8 errorHtml
  where
    successHtml =
      "<!DOCTYPE html>\
      \<html>\
      \<head><title>Email Verified</title></head>\
      \<body style='font-family: Arial; text-align: center; padding: 50px;'>\
      \<h1 style='color: green;'>✓ Email Verified Successfully!</h1>\
      \<p>Your business email has been verified.</p>\
      \<p>You can now close this window and return to the app.</p>\
      \</body>\
      \</html>"

    errorHtml =
      "<!DOCTYPE html>\
      \<html>\
      \<head><title>Verification Failed</title></head>\
      \<body style='font-family: Arial; text-align: center; padding: 50px;'>\
      \<h1 style='color: red;'>✗ Verification Failed</h1>\
      \<p>The verification link is invalid or has expired.</p>\
      \<p>Please request a new verification email from the app.</p>\
      \</body>\
      \</html>"

-- Resend business email verification OTP
resendBusinessEmailVerification :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler APISuccess
resendBusinessEmailVerification (personId, merchantId) = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  DRegistration.resendBusinessEmailVerification personId merchantId merchantOperatingCityId
