{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Registration
  ( DRegistration.AuthRawReq (..),
    DRegistration.AuthReq (..),
    DRegistration.AuthRes (..),
    DRegistration.ResendAuthRes,
    DRegistration.AuthVerifyReq (..),
    DRegistration.AuthVerifyRes (..),
    API,
    handler,
  )
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NE
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.RegistrationToken as SRT
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Network.HTTP.Media as M
import Servant hiding (throwError)
import Tools.Auth (TokenAuth)

data JSONBS deriving (Typeable)

instance Accept JSONBS where
  contentTypes _ =
    "application" M.// "json" M./: ("charset", "utf-8")
      NE.:| ["application" M.// "json"]

instance MimeRender JSONBS DRegistration.AuthRawReq where
  mimeRender _ (DRegistration.AuthRawReq bs) = BSL.fromStrict bs

instance MimeUnrender JSONBS DRegistration.AuthRawReq where
  mimeUnrender _ = pure . DRegistration.AuthRawReq . BSL.toStrict

---- Registration Flow ------
type API =
  "auth"
    :> ( ReqBody '[JSONBS] DRegistration.AuthRawReq --ReqBody '[JSON] DRegistration.AuthReq
           :> Header "x-sdk-authorization" Text
           :> Header "x-bundle-version" Version
           :> Header "x-client-version" Version
           :> Post '[JSON] DRegistration.AuthRes
           :<|> Capture "authId" (Id SRT.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] DRegistration.AuthVerifyReq
             :> Post '[JSON] DRegistration.AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SRT.RegistrationToken)
             :> "resend"
             :> Post '[JSON] DRegistration.ResendAuthRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  auth
    :<|> verify
    :<|> resend
    :<|> logout

auth :: DRegistration.AuthRawReq -> Maybe Text -> Maybe Version -> Maybe Version -> FlowHandler DRegistration.AuthRes
auth rawReq mbSignature mbBundleVersion =
  withFlowHandlerAPI . DRegistration.auth rawReq mbSignature mbBundleVersion

verify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> FlowHandler DRegistration.AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DRegistration.verify tokenId

resend :: Id SR.RegistrationToken -> FlowHandler DRegistration.ResendAuthRes
resend = withFlowHandlerAPI . DRegistration.resend

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DRegistration.logout personId
