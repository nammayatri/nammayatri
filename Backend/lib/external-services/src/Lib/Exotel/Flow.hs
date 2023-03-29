{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Exotel.Flow where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import Lib.Error
import Lib.Exotel.Types
import Servant hiding (throwError)
import Servant.Client

-- | Exotel API interface
type ExotelConnectAPI =
  BasicAuth "apikey-apitoken" BasicAuthData
    :> ReqBody '[FormUrlEncoded] ExotelRequest
    :> Post '[JSON] ExotelResponse

exotelConnectAPI :: Proxy ExotelConnectAPI
exotelConnectAPI = Proxy

defaultBaseUrl :: ExotelAccountSID -> BaseUrl
defaultBaseUrl sid =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.exotel.com",
      baseUrlPort = 443,
      baseUrlPath =
        T.unpack $
          "/v1/Accounts/"
            <> getExotelAccountSID sid
            <> "/Calls/connect.json"
    }

initiateCall ::
  ( CoreMetrics m,
    HasFlowEnv m r '["exotelCfg" ::: Maybe ExotelCfg]
  ) =>
  T.Text ->
  T.Text ->
  BaseUrl ->
  ExotelAttachments ->
  m ExotelResponse
initiateCall from to callbackUrl attachments = do
  withLogTag "Exotel" $ do
    ExotelCfg {..} <- asks (.exotelCfg) >>= fromMaybeM ExotelNotConfigured
    let exoRequest = ExotelRequest from to (getExotelCallerId callerId) callbackUrl attachments
        authData =
          BasicAuthData
            (DT.encodeUtf8 $ getExotelApiKey apiKey)
            (DT.encodeUtf8 $ getExotelApiToken apiToken)
    callExotelAPI
      (defaultBaseUrl sid)
      (callExotel authData exoRequest)
      "initiateCall"
  where
    callExotel authData exoRequest = ET.client exotelConnectAPI authData exoRequest

callExotelAPI :: CallAPI env a
callExotelAPI =
  callApiUnwrappingApiError
    (identity @ExotelError)
    Nothing
    (Just "EXOTEL_NOT_AVAILABLE")
