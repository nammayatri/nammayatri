{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Whatsapp.GupShup.Flow where

import qualified Data.Text as T
import EulerHS.Types (EulerClient, client)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Error
import qualified Lib.Whatsapp.Interface.Types as Whatsapp
import Servant hiding (throwError)
import Servant.Client.Core (ClientError)

type GupShupAPI =
  GupShupOptAPI
    :<|> GupShupSendOtpAPI

type GupShupOptAPI =
  MandatoryQueryParam "userid" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "phone_number" Text
    :> MandatoryQueryParam "method" Text
    :> MandatoryQueryParam "auth_scheme" Text
    :> MandatoryQueryParam "v" Text
    :> MandatoryQueryParam "channel" Text
    :> MandatoryQueryParam "format" Text
    :> Get '[JSON] Whatsapp.OptApiResp

type GupShupSendMessageWithTemplateIdAPI = GupShupSendOtpAPI

type GupShupSendOtpAPI =
  MandatoryQueryParam "userid" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "send_to" Text
    :> MandatoryQueryParam "method" Text
    :> MandatoryQueryParam "auth_scheme" Text
    :> MandatoryQueryParam "v" Text
    :> MandatoryQueryParam "msg_type" Text
    :> MandatoryQueryParam "format" Text
    :> MandatoryQueryParam "var1" Text
    :> MandatoryQueryParam "template_id" Text
    :> Get '[JSON] Whatsapp.SendOtpApiResp

gupShupOptAPIClient :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> EulerClient Whatsapp.OptApiResp
gupShupSendOtpAPIClient :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> EulerClient Whatsapp.SendOtpApiResp
gupShupOptAPIClient :<|> gupShupSendOtpAPIClient = client (Proxy :: Proxy GupShupAPI)

whatsAppOptAPI ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m Whatsapp.OptApiResp
whatsAppOptAPI url userid password phone_number method auth_scheme v channel format = do
  callAPI url (gupShupOptAPIClient userid password phone_number method auth_scheme v channel format) "GupShup Opt api"
    >>= checkGupShupOptError url

checkGupShupOptError :: (MonadThrow m, Log m, HasField "_response" a Whatsapp.OptApiResponse) => BaseUrl -> Either ClientError a -> m a
checkGupShupOptError url res =
  fromEitherM (gupShupOptError url) res >>= validateResponseStatus

gupShupOptError :: BaseUrl -> ClientError -> ExternalAPICallError
gupShupOptError = ExternalAPICallError (Just "GUPSHUP_OPT_API_ERROR")

validateResponseStatus :: (MonadThrow m, Log m, HasField "_response" a Whatsapp.OptApiResponse) => a -> m a
validateResponseStatus response = do
  let resp = response._response
  case T.toLower resp.status of
    "success" -> pure response
    _ -> do
      case resp.id of
        "100" -> throwError GupShupUnknownServerError
        "101" -> throwError GupShupInvalidRequest
        "102" -> throwError GupShupUnauthorized
        "103" -> throwError GupShupUserIdNotFound
        "105" -> throwError GupShupInvalidPhoneNumber
        "106" -> throwError GupShupWrongMethodService
        "175" -> throwError GupShupInterNationalPhoneNumber
        "322" -> throwError GupShupTooManyRequests
        _ -> throwError GupShupUnknownServerError

whatsAppSendOtpAPI ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  m Whatsapp.SendOtpApiResp
whatsAppSendOtpAPI url userid password sendTo method auth_scheme v msgType format var1 templateId = do
  callAPI url (gupShupSendOtpAPIClient userid password sendTo method auth_scheme v msgType format var1 templateId) "GupShup Otp api"
    >>= checkGupShupOptError url
