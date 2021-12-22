module Beckn.External.Exotel.Flow where

import Beckn.External.Exotel.Types
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import Beckn.Utils.Common
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import qualified EulerHS.Types as ET
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
  m ()
initiateCall from to = do
  withLogTag "Exotel" $ do
    ExotelCfg {..} <- asks (.exotelCfg) >>= fromMaybeM ExotelNotConfigured
    let exoRequest = ExotelRequest from to $ getExotelCallerId callerId
        authData =
          BasicAuthData
            (DT.encodeUtf8 $ getExotelApiKey apiKey)
            (DT.encodeUtf8 $ getExotelApiToken apiToken)
    callExotelAPI
      (defaultBaseUrl sid)
      (callExotel authData exoRequest)
      "initiateCall"
  where
    callExotel authData exoRequest = void $ ET.client exotelConnectAPI authData exoRequest

callExotelAPI :: CallAPI env a
callExotelAPI =
  callApiUnwrappingApiError
    (identity @ExotelError)
    Nothing
    (Just "EXOTEL_NOT_AVAILABLE")
